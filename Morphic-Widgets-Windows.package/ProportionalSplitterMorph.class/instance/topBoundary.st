accessing
topBoundary
	"Answer the top boundary position by calculating the minimum
	of the minimum heights of the top morphs."
	
	|morphs|
	morphs := leftOrTop reject: [:m |
		m layoutFrame topFraction ~= 0 and: [
			m layoutFrame topFraction = m layoutFrame bottomFraction]].
	morphs ifEmpty: [
		^(self splitterAbove
			ifNil: [self containingWindow panelRect top]
			ifNotNil: [:s | s first top]) + 25].
	^(morphs collect: [:m |
		m top + m minExtent y +
			(self layoutFrame topOffset ifNil: [0]) -
			(m layoutFrame bottomOffset ifNil: [0])]) max