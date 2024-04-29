package token

import "fmt"

type Position struct {
	Filename string
	Offset   int
	Line     int
	Column   int
}

func (p Position) String() string {
	if p.Filename == "" {
		return ""
	}
	if p.Line == 0 {
		return p.Filename
	}

	return p.Filename + ":" + fmt.Sprintf("%d:%d", p.Line, p.Column)
}

func (p Position) IsValid() bool {
	return p.Filename != "" || p.Offset > 0
}

type Pos int

func (p Pos) IsValid() bool {
	return p != 0
}

const NoPos Pos = 0

type File struct {
	name string
	size int

	line []int
}

func NewFile(name string, base, size int) *File {
	return &File{
		name: name,
		size: size,
		line: []int{0},
	}
}

// Name returns the file name of file f as registered with AddFile.
func (f *File) Name() string {
	return f.name
}

// Size returns the size of file f as registered with AddFile.
func (f *File) Size() int {
	return f.size
}

func (f *File) AddLine(offset int) {
	f.line = append(f.line, offset)
}

func (f *File) SetLines(lines []int) {
	f.line = lines
}

func (f *File) Position(pos Pos) Position {
	if pos == NoPos {
		return Position{
			Filename: f.name,
			Offset:   0,
			Line:     1,
			Column:   1,
		}
	}
	if pos < 0 || pos > Pos(f.size) {
		return Position{Filename: f.name, Offset: int(pos), Line: 0, Column: 0}
	}
	// determine line by binary search
	line := 1
	i, j := 0, len(f.line)
	for i < j {
		h := i + (j-i)/2
		if f.line[h] <= int(pos) {
			line = h + 1
			i = h + 1
		} else {
			j = h
		}
	}

	column := int(pos) - f.line[line-1] + 1
	return Position{Filename: f.name, Offset: int(pos), Line: line, Column: column}
}
