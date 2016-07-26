private
findSatisfying: aBlock in: aMorph
	"Explores aMorph hierarchy of sub morphs to find a morph who satisfies aBlock"
	aMorph allMorphsDo: [:each | (aBlock value: each) ifTrue: [^ each]].
	self fail.