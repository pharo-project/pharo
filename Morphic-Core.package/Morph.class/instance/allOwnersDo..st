structure
allOwnersDo: aBlock
	"Evaluate aBlock with all owners of the receiver"
	owner ifNotNil:[^owner withAllOwnersDo: aBlock].