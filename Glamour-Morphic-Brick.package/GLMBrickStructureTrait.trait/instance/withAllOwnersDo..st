brick-structure
withAllOwnersDo: aBlock
	"Evaluate aBlock with the receiver and all of its owners"
	aBlock cull: self.
	self owner ifNotNil:[ ^ self owner withAllOwnersDo: aBlock ]