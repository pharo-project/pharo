accessing
rightBoundary
	"Answer the right boundary position by calculating the minimum
	of the minimum widths of the right morphs."
	
	|morphs|
	morphs := rightOrBottom reject: [:m |
		m layoutFrame rightFraction ~= 1 and: [
			m layoutFrame leftFraction = m layoutFrame rightFraction]].
	morphs ifEmpty: [
		^(self splitterRight
			ifNil: [self containingWindow panelRect right]
			ifNotNil: [:s | s left]) + 25].
	^(morphs collect: [:m |
		m right - m minExtent x -
			(m layoutFrame leftOffset ifNil: [0]) +
			(self layoutFrame rightOffset ifNil: [0])]) min - self class splitterWidth