package scanner

import (
	"bytes"
	"fmt"
	"path/filepath"
	"unicode"
	"unicode/utf8"

	"github.com/danecwalker/partial/token"
)

type scanner struct {
	file *token.File // source file handle
	dir  string      // directory portion of file.Name()
	src  []byte      // source

	// scanning state
	ch         rune      // current character
	offset     int       // character offset
	rdOffset   int       // reading offset (position after current character)
	lineOffset int       // current line offset
	insertSemi bool      // insert a semicolon before next newline
	nlPos      token.Pos // position of newline in preceding comment
	errorCount int       // number of errors encountered
	errors     []error   // errors encountered
}

const (
	bom = 0xFEFF // byte order mark, only permitted as very first character
	eof = -1     // end of file
)

func (s *scanner) Err() {
	if s.errorCount > 0 {
		for _, err := range s.errors {
			fmt.Println(err)
		}
	}
}

func (s *scanner) error(offset int, msg string) {
	pos := s.file.Position(token.Pos(offset))
	s.errors = append(s.errors, fmt.Errorf("%s: %s", pos, msg))
	s.errorCount++
}

func (s *scanner) errorf(offset int, format string, args ...interface{}) {
	s.error(offset, fmt.Sprintf(format, args...))
}

func (s *scanner) next() {
	if s.rdOffset < len(s.src) {
		s.offset = s.rdOffset
		if s.ch == '\n' {
			s.lineOffset = s.offset
			s.file.AddLine(s.offset)
		}
		r, w := rune(s.src[s.rdOffset]), 1
		switch {
		case r == 0:
			s.error(s.offset, "illegal character NUL")
		case r >= utf8.RuneSelf:
			// not ASCII
			r, w = utf8.DecodeRune(s.src[s.rdOffset:])
			if r == utf8.RuneError && w == 1 {
				s.error(s.offset, "illegal UTF-8 encoding")
			} else if r == bom && s.offset > 0 {
				s.error(s.offset, "illegal byte order mark")
			}
		}
		s.rdOffset += w
		s.ch = r
	} else {
		s.offset = len(s.src)
		if s.ch == '\n' {
			s.lineOffset = s.offset
			s.file.AddLine(s.offset)
		}
		s.ch = eof
	}
}

func (s *scanner) peek() byte {
	if s.rdOffset < len(s.src) {
		return s.src[s.rdOffset]
	}
	return 0
}

func NewScanner(file *token.File, src []byte) *scanner {
	if file.Size() != len(src) {
		panic(fmt.Sprintf("file size (%d) does not match src len (%d)", file.Size(), len(src)))
	}

	s := new(scanner)

	s.file = file
	s.dir, _ = filepath.Split(file.Name())
	s.src = src

	s.ch = ' '
	s.offset = 0
	s.rdOffset = 0
	s.lineOffset = 0
	s.insertSemi = false
	s.errorCount = 0

	s.next() // initializes s.ch
	if s.ch == bom {
		s.next() // consume BOM
	}

	return s
}

func (s *scanner) scanComment() (string, int) {
	offs := s.offset - 1 // position of initial '/'
	next := -1           // position immediately following the comment
	numCR := 0
	nlOffs := 0

	if s.ch == '/' {
		s.next()
		for s.ch != '\n' && s.ch >= 0 {
			if s.ch == '\r' {
				numCR++
			}
			s.next()
		}

		next = s.offset
		if s.ch == '\n' {
			next++
		}
		goto exit
	}

	s.next()
	for s.ch >= 0 {
		ch := s.ch
		if ch == '\r' {
			numCR++
		} else if ch == '\n' && nlOffs == 0 {
			nlOffs = s.offset
		}
		s.next()
		if ch == '*' && s.ch == '/' {
			s.next()
			next = s.offset
			goto exit
		}
	}

	s.error(offs, "comment not terminated")

exit:
	lit := s.src[offs:s.offset]

	if numCR > 0 && len(lit) >= 2 && lit[1] == '/' && lit[len(lit)-1] == '\r' {
		lit = lit[:len(lit)-1]
		numCR--
	}

	if next >= 0 && (lit[1] == '*' || offs == s.lineOffset) && bytes.HasPrefix(lit[2:], prefix) {
		// line info
	}

	if numCR > 0 {
		lit = stripCR(lit, lit[1] == '*')
	}

	return string(lit), nlOffs
}

var prefix = []byte("line ")

func stripCR(b []byte, comment bool) []byte {
	c := make([]byte, len(b))
	i := 0
	for j, ch := range b {
		if ch != '\r' || comment && i > len("/*") && c[i-1] == '*' && j+1 < len(b) && b[j+1] == '/' {
			c[i] = ch
			i++
		}
	}
	return c[:i]
}

func (s *scanner) skipWhitespace() {
	for s.ch == ' ' || s.ch == '\t' || s.ch == '\n' && !s.insertSemi || s.ch == '\r' {
		s.next()
	}
}

func digitVal(ch rune) int {
	switch {
	case '0' <= ch && ch <= '9':
		return int(ch - '0')
	case 'a' <= lower(ch) && lower(ch) <= 'f':
		return int(lower(ch) - 'a' + 10)
	}
	return 16 // larger than any legal digit val
}

func lower(ch rune) rune     { return ('a' - 'A') | ch } // returns lower-case ch iff ch is ASCII letter
func isDecimal(ch rune) bool { return '0' <= ch && ch <= '9' }
func isHex(ch rune) bool     { return '0' <= ch && ch <= '9' || 'a' <= lower(ch) && lower(ch) <= 'f' }

func isLetter(ch rune) bool {
	return 'a' <= lower(ch) && lower(ch) <= 'z' || ch == '_' || ch >= utf8.RuneSelf && unicode.IsLetter(ch)
}

func isDigit(ch rune) bool {
	return isDecimal(ch) || ch >= utf8.RuneSelf && unicode.IsDigit(ch)
}

func (s *scanner) scanIdentifier() string {
	offs := s.offset
	for rdOffset, b := range s.src[s.rdOffset:] {
		if 'a' <= b && b <= 'z' || 'A' <= b && b <= 'Z' || b == '_' || '0' <= b && b <= '9' {
			continue
		}
		s.rdOffset += rdOffset
		if 0 < b && b < utf8.RuneSelf {
			s.ch = rune(b)
			s.offset = s.rdOffset
			s.rdOffset++
			goto exit
		}
		s.next()
		for isLetter(s.ch) || isDigit(s.ch) {
			s.next()
		}
		goto exit
	}
	s.offset = len(s.src)
	s.rdOffset = len(s.src)
	s.ch = eof

exit:
	return string(s.src[offs:s.offset])
}

func (s *scanner) digits(base int, invalid *int) (digsep int) {
	if base <= 10 {
		max := rune('0' + base)
		for isDecimal(s.ch) || s.ch == '_' {
			ds := 1
			if s.ch == '_' {
				ds = 2
			} else if s.ch >= max && *invalid < 0 {
				*invalid = s.offset // record invalid rune offset
			}
			digsep |= ds
			s.next()
		}
	} else {
		for isHex(s.ch) || s.ch == '_' {
			ds := 1
			if s.ch == '_' {
				ds = 2
			}
			digsep |= ds
			s.next()
		}
	}
	return
}

func (s *scanner) scanNumber() (token.Token, string) {
	offs := s.offset
	tok := token.ILLEGAL

	base := 10        // number base
	prefix := rune(0) // one of 0 (decimal), '0' (0-octal), 'x', 'o', or 'b'
	digsep := 0       // bit 0: digit present, bit 1: '_' present
	invalid := -1     // index of invalid digit in literal, or < 0

	// integer part
	if s.ch != '.' {
		tok = token.INT
		if s.ch == '0' {
			s.next()
			switch lower(s.ch) {
			case 'x':
				s.next()
				base, prefix = 16, 'x'
			case 'o':
				s.next()
				base, prefix = 8, 'o'
			case 'b':
				s.next()
				base, prefix = 2, 'b'
			default:
				base, prefix = 8, '0'
				digsep = 1 // leading 0
			}
		}
		digsep |= s.digits(base, &invalid)
	}

	// fractional part
	if s.ch == '.' {
		tok = token.FLOAT
		if prefix == 'o' || prefix == 'b' {
			s.error(s.offset, "invalid radix point in "+litname(prefix))
		}
		s.next()
		digsep |= s.digits(base, &invalid)
	}

	if digsep&1 == 0 {
		s.error(s.offset, litname(prefix)+" has no digits")
	}

	// exponent
	if e := lower(s.ch); e == 'e' || e == 'p' {
		switch {
		case e == 'e' && prefix != 0 && prefix != '0':
			s.errorf(s.offset, "%q exponent requires decimal mantissa", s.ch)
		case e == 'p' && prefix != 'x':
			s.errorf(s.offset, "%q exponent requires hexadecimal mantissa", s.ch)
		}
		s.next()
		tok = token.FLOAT
		if s.ch == '+' || s.ch == '-' {
			s.next()
		}
		ds := s.digits(10, nil)
		digsep |= ds
		if ds&1 == 0 {
			s.error(s.offset, "exponent has no digits")
		}
	} else if prefix == 'x' && tok == token.FLOAT {
		s.error(s.offset, "hexadecimal mantissa requires a 'p' exponent")
	}

	// suffix 'i'
	if s.ch == 'i' {
		tok = token.IMAG
		s.next()
	}

	lit := string(s.src[offs:s.offset])
	if tok == token.INT && invalid >= 0 {
		s.errorf(invalid, "invalid digit %q in %s", lit[invalid-offs], litname(prefix))
	}
	if digsep&2 != 0 {
		if i := invalidSep(lit); i >= 0 {
			s.error(offs+i, "'_' must separate successive digits")
		}
	}

	return tok, lit
}

func invalidSep(x string) int {
	x1 := ' ' // prefix char, we only care if it's 'x'
	d := '.'  // digit, one of '_', '0' (a digit), or '.' (anything else)
	i := 0

	// a prefix counts as a digit
	if len(x) >= 2 && x[0] == '0' {
		x1 = lower(rune(x[1]))
		if x1 == 'x' || x1 == 'o' || x1 == 'b' {
			d = '0'
			i = 2
		}
	}

	// mantissa and exponent
	for ; i < len(x); i++ {
		p := d // previous digit
		d = rune(x[i])
		switch {
		case d == '_':
			if p != '0' {
				return i
			}
		case isDecimal(d) || x1 == 'x' && isHex(d):
			d = '0'
		default:
			if p == '_' {
				return i - 1
			}
			d = '.'
		}
	}
	if d == '_' {
		return len(x) - 1
	}

	return -1
}

func litname(prefix rune) string {
	switch prefix {
	case 'x':
		return "hexadecimal literal"
	case 'o', '0':
		return "octal literal"
	case 'b':
		return "binary literal"
	}
	return "decimal literal"
}

func (s *scanner) switch2(tok0, tok1 token.Token) token.Token {
	if s.ch == '=' {
		s.next()
		return tok1
	}
	return tok0
}

func (s *scanner) switch3(tok0, tok1 token.Token, ch2 rune, tok2 token.Token) token.Token {
	if s.ch == '=' {
		s.next()
		return tok1
	}
	if s.ch == ch2 {
		s.next()
		return tok2
	}
	return tok0
}

func (s *scanner) switch4(tok0, tok1 token.Token, ch2 rune, tok2, tok3 token.Token) token.Token {
	if s.ch == '=' {
		s.next()
		return tok1
	}
	if s.ch == ch2 {
		s.next()
		if s.ch == '=' {
			s.next()
			return tok3
		}
		return tok2
	}
	return tok0
}

func (s *scanner) Scan() (pos token.Pos, tok token.Token, lit string) {
	if s.nlPos.IsValid() {
		pos, tok, lit = s.nlPos, token.SEMICOLON, "\n"
		s.nlPos = token.NoPos
		return
	}

	s.skipWhitespace()

	pos = token.Pos(s.offset)

	insertSemi := false
	switch ch := s.ch; {
	case isLetter(ch):
		lit = s.scanIdentifier()
		if len(lit) > 1 {
			tok = token.Lookup(lit)
			switch tok {
			case token.IDENT, token.BREAK, token.CONTINUE, token.FALLTHROUGH, token.RETURN:
				insertSemi = true
			}
		} else {
			insertSemi = true
			tok = token.IDENT
		}
	case isDecimal(ch) || ch == '.' && isDecimal(rune(s.peek())):
		insertSemi = true
		tok, lit = s.scanNumber()
	default:
		s.next()
		switch ch {
		case eof:
			if s.insertSemi {
				s.insertSemi = false
				return pos, token.SEMICOLON, "\n"
			}
			tok = token.EOF
		case '\n':
			s.insertSemi = false
			return pos, token.SEMICOLON, "\n"
		case '"':
			insertSemi = true
			tok = token.STRING
			lit = s.scanString()
		case '\'':
			insertSemi = true
			tok = token.CHAR
			lit = s.scanRune()
		case '`':
			insertSemi = true
			tok = token.STRING
			lit = s.scanRawString()
		case ':':
			tok = s.switch2(token.COLON, token.DEFINE)
		case '.':
			// fractions starting with a '.' are handled by outer switch
			tok = token.PERIOD
			if s.ch == '.' && s.peek() == '.' {
				s.next()
				s.next() // consume last '.'
				tok = token.ELLIPSIS
			}
		case ',':
			tok = token.COMMA
		case ';':
			tok = token.SEMICOLON
			lit = ";"
		case '(':
			tok = token.LPAREN
		case ')':
			insertSemi = true
			tok = token.RPAREN
		case '[':
			tok = token.LBRACK
		case ']':
			insertSemi = true
			tok = token.RBRACK
		case '{':
			tok = token.LBRACE
		case '}':
			insertSemi = true
			tok = token.RBRACE
		case '+':
			tok = s.switch3(token.ADD, token.ADD_ASSIGN, '+', token.INC)
			if tok == token.INC {
				insertSemi = true
			}
		case '-':
			tok = s.switch3(token.SUB, token.SUB_ASSIGN, '-', token.DEC)
			if tok == token.DEC {
				insertSemi = true
			}
		case '*':
			tok = s.switch2(token.MUL, token.MUL_ASSIGN)
		case '/':
			if s.ch == '/' || s.ch == '*' {
				// comment
				comment, nlOffset := s.scanComment()
				if s.insertSemi && nlOffset != 0 {
					// For /*...*/ containing \n, return
					// COMMENT then artificial SEMICOLON.
					s.nlPos = token.Pos(nlOffset)
					s.insertSemi = false
				} else {
					insertSemi = s.insertSemi // preserve insertSemi info
				}
				tok = token.COMMENT
				lit = comment
			} else {
				// division
				tok = s.switch2(token.QUO, token.QUO_ASSIGN)
			}
		case '%':
			tok = s.switch2(token.REM, token.REM_ASSIGN)
		case '^':
			tok = s.switch2(token.XOR, token.XOR_ASSIGN)
		case '<':
			if s.ch == '-' {
				s.next()
				tok = token.ARROW
			} else {
				tok = s.switch4(token.LSS, token.LEQ, '<', token.SHL, token.SHL_ASSIGN)
			}
		case '>':
			tok = s.switch4(token.GTR, token.GEQ, '>', token.SHR, token.SHR_ASSIGN)
		case '=':
			tok = s.switch2(token.ASSIGN, token.EQL)
		case '!':
			tok = s.switch2(token.NOT, token.NEQ)
		case '&':
			if s.ch == '^' {
				s.next()
				tok = s.switch2(token.AND_NOT, token.AND_NOT_ASSIGN)
			} else {
				tok = s.switch3(token.AND, token.AND_ASSIGN, '&', token.LAND)
			}
		case '|':
			tok = s.switch3(token.OR, token.OR_ASSIGN, '|', token.LOR)
		case '~':
			tok = token.TILDE
		default:
			// next reports unexpected BOMs - don't repeat
			if ch != bom {
				// Report an informative error for U+201[CD] quotation
				// marks, which are easily introduced via copy and paste.
				if ch == '“' || ch == '”' {
					s.errorf(int(pos), "curly quotation mark %q (use neutral %q)", ch, '"')
				} else {
					s.errorf(int(pos), "illegal character %#U", ch)
				}
			}
			insertSemi = s.insertSemi // preserve insertSemi info
			tok = token.ILLEGAL
			lit = string(ch)
		}
	}

	s.insertSemi = insertSemi

	return
}

func (s *scanner) scanRawString() string {
	// '`' opening already consumed
	offs := s.offset - 1

	hasCR := false
	for {
		ch := s.ch
		if ch < 0 {
			s.error(offs, "raw string literal not terminated")
			break
		}
		s.next()
		if ch == '`' {
			break
		}
		if ch == '\r' {
			hasCR = true
		}
	}

	lit := s.src[offs:s.offset]
	if hasCR {
		lit = stripCR(lit, false)
	}

	return string(lit)
}

func (s *scanner) scanRune() string {
	// '\'' opening already consumed
	offs := s.offset - 1

	valid := true
	n := 0
	for {
		ch := s.ch
		if ch == '\n' || ch < 0 {
			// only report error if we don't have one already
			if valid {
				s.error(offs, "rune literal not terminated")
				valid = false
			}
			break
		}
		s.next()
		if ch == '\'' {
			break
		}
		n++
		if ch == '\\' {
			if !s.scanEscape('\'') {
				valid = false
			}
			// continue to read to closing quote
		}
	}

	if valid && n != 1 {
		s.error(offs, "illegal rune literal")
	}

	return string(s.src[offs:s.offset])
}

func (s *scanner) scanString() string {
	// '"' opening already consumed
	offs := s.offset - 1

	for {
		ch := s.ch
		if ch == '\n' || ch < 0 {
			s.error(offs, "string literal not terminated")
			break
		}
		s.next()
		if ch == '"' {
			break
		}
		if ch == '\\' {
			s.scanEscape('"')
		}
	}

	return string(s.src[offs:s.offset])
}

func (s *scanner) scanEscape(quote rune) bool {
	offs := s.offset

	var n int
	var base, max uint32
	switch s.ch {
	case 'a', 'b', 'f', 'n', 'r', 't', 'v', '\\', quote:
		s.next()
		return true
	case '0', '1', '2', '3', '4', '5', '6', '7':
		n, base, max = 3, 8, 255
	case 'x':
		s.next()
		n, base, max = 2, 16, 255
	case 'u':
		s.next()
		n, base, max = 4, 16, unicode.MaxRune
	case 'U':
		s.next()
		n, base, max = 8, 16, unicode.MaxRune
	default:
		msg := "unknown escape sequence"
		if s.ch < 0 {
			msg = "escape sequence not terminated"
		}
		s.error(offs, msg)
		return false
	}

	var x uint32
	for n > 0 {
		d := uint32(digitVal(s.ch))
		if d >= base {
			msg := fmt.Sprintf("illegal character %#U in escape sequence", s.ch)
			if s.ch < 0 {
				msg = "escape sequence not terminated"
			}
			s.error(s.offset, msg)
			return false
		}
		x = x*base + d
		s.next()
		n--
	}

	if x > max || 0xD800 <= x && x < 0xE000 {
		s.error(offs, "escape sequence is invalid Unicode code point")
		return false
	}

	return true
}
