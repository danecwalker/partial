package main

import (
	"fmt"
	"os"

	"github.com/danecwalker/partial/scanner"
	"github.com/danecwalker/partial/token"
)

func main() {
	src, _ := os.ReadFile("examples/01.svelte")
	file := token.NewFile("examples/01.svelte", 0, len(src))
	s := scanner.NewScanner(file, src)
	for {
		pos, tok, lit := s.Scan()
		fmt.Printf("%s: %s %q\n", file.Position(pos), tok, lit)
		if tok == token.EOF {
			break
		}
	}

	s.Err()
}
