instance creation
privateFinalizingNew
	"Answer a new instance that will free its page when garbage collected."
	| address |
	address := self unusedBlockInExecutablePage.
	^AllocatedThunks
		add: (self forPointer: address)
		finalizing: address 