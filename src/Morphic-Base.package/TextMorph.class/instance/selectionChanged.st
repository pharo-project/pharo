private
selectionChanged
	"Invalidate all the selection rectangles. 
	Make sure that any drop shadow is accounted for too."
	self paragraph selectionRects
		do: [:r | | intr |
			intr := r intersect: self fullBounds ifNone: [ nil ].
			intr ifNotNil: [ self invalidRect: (self expandFullBoundsForDropShadow: intr)] ]