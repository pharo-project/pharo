private
lines
	"Compute lines based on logical line breaks, not optical (which may change due to line wrapping of the editor).
	Subclasses using kinds of Paragraphs can instead use the service provided by it.
	"
	| lines string index lineIndex stringSize |
	string := self string.
	"Empty strings have no lines at all. Think of something."
	string isEmpty ifTrue:[^{#(1 0 0)}].
	stringSize := string size.
	lines := OrderedCollection new: (string size // 15).
	index := 0.
	lineIndex := 0.
	string linesDo:[:line |
		lines addLast: (Array
			with: (index := index + 1)
			with: (lineIndex := lineIndex + 1)
			with: (index := index + line size min: stringSize))].
	"Special workaround for last line empty."
	string last == Character cr
	"lines last last < stringSize" ifTrue:[lines addLast:{stringSize +1. lineIndex+1. stringSize}].
	^lines