*Polymorph-Widgets
overlapsHorizontal: aSplitter
	"Answer whether the receiver overlaps the given spiltter
	in the horizontal plane."

	^aSplitter left <= self right and: [aSplitter right >= self left]