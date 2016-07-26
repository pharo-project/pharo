pointers management
registerPointer: aPointer
	"Register a pointer so that it is free automatically in #freePointers"
	self pointers add: aPointer