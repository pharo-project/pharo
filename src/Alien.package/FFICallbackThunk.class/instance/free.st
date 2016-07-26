memory management
free
	"Recycle the memory in the executable page containing the receiver's thunk.
	 Since the receiver is a derived pointer to a block in an executable page this
	 requires returning the block to the free pool."
	AllocatedThunks remove: self.
	AccessProtect critical:
		[| address |
		address := self addressField.
		address ~= 0 ifTrue:
			[self addressFieldPut: 0.
			 self class freeBlockInExecutablePage: address]]