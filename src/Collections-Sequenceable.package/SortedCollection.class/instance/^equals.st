comparing
= aSortedCollection
	"Answer true if my and aSortedCollection's species are the same,
	and if our blocks are the same, and if our elements are the same."

	self species = aSortedCollection species ifFalse: [^ false].
	sortBlock = aSortedCollection sortBlock
		ifTrue: [^ super = aSortedCollection]
		ifFalse: [^ false]