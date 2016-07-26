private
findLastSatisfying: aBlock in: aMorph
	"Explores aMorph hierarchy of sub morphs to find the last morph who satisfies aBlock"
	| lastMatch |
	lastMatch := nil. 
	aMorph allMorphsDo: [:each | (aBlock value: each) ifTrue: [lastMatch := each]].
	^ lastMatch ifNil: [self fail]