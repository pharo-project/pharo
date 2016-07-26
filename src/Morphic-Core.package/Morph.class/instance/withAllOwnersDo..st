structure
withAllOwnersDo: aBlock
	"Evaluate aBlock with the receiver and all of its owners"
	aBlock value: self.
	owner ifNotNil:[^owner withAllOwnersDo: aBlock].