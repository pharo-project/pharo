accessing
bottomBoundary
	"Answer the bottom boundary position by calculating the minimum
	of the minimum heights of the bottom morphs."
	
	|morphs|
	morphs := rightOrBottom reject: [:m |
		m layoutFrame bottomFraction ~= 1 and: [
			m layoutFrame topFraction = m layoutFrame bottomFraction]].
	morphs ifEmpty: [
		^(self splitterBelow
			ifNil: [self containingWindow panelRect bottom]
			ifNotNil: [self splitterBelow first top]) - 25].
	^(morphs collect: [:m |
		m bottom - m minExtent y -
			(m layoutFrame topOffset ifNil: [0]) +
			(self layoutFrame bottomOffset ifNil: [0])]) min - self class splitterWidth