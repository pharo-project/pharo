copying
grownBy: length
	"Answer a copy of receiver collection with size grown by length"

	| newCollection size |
	size := self size.
	newCollection := self species ofSize: size + length.
	newCollection replaceFrom: 1 to: size with: self startingAt: 1.
	^ newCollection