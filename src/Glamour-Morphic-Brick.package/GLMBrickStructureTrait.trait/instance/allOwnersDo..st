brick-structure
allOwnersDo: aBlock
	"Evaluate aBlock with all owners of the receiver"
	self owner ifNotNil:[ ^ self owner withAllOwnersDo: aBlock ].