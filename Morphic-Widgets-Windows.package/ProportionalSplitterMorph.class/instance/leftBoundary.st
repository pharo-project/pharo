accessing
leftBoundary
	"Answer the left boundary position by calculating the minimum
	of the minimum widths of the left morphs."
	
	|morphs|
	morphs := leftOrTop reject: [:m |
		m layoutFrame leftFraction ~= 0 and: [
			m layoutFrame leftFraction = m layoutFrame rightFraction]].
	morphs ifEmpty: [
		^(self splitterLeft
			ifNil: [self containingWindow panelRect left]
			ifNotNil: [:s | s left]) + 25].
	^(morphs collect: [:m |
		m left + m minExtent x +
			(self layoutFrame leftOffset ifNil: [0]) -
			(m layoutFrame rightOffset ifNil: [0])]) max