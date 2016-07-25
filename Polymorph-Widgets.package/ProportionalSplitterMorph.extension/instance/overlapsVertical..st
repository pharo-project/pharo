*Polymorph-Widgets
overlapsVertical: aSplitter
	"Answer whether the receiver overlaps the given spiltter
	in the vertical plane."

	^aSplitter top <= self bottom and: [aSplitter bottom >= self top]