*Text-Core
encompassParagraph: anInterval
	"Return an interval that includes anInterval, and that comprises one or several whole paragraphs in the receiver.
	Answer starts at the position following a cr (or eventually 1) and ends at a cr (or eventually at self size).
	See also #encompassLine:"
	| left right |
	left := (self lastIndexOf: Character cr startingAt: anInterval first - 1 ifAbsent:[0]) + 1.
	right := (self indexOf: Character cr startingAt: (anInterval last max: anInterval first) ifAbsent: [self size]).
	^left to: right