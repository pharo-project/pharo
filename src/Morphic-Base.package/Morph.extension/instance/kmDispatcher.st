*Morphic-Base-keymapping
kmDispatcher
	"When creating the KMDispatcher instance, load default shortcuts."

	^ self
		valueOfProperty: #kmDispatcher
		ifAbsentPut: [ 
			| kmd |
			kmd := KMDispatcher target: self.
			self initializeShortcuts: kmd.
			kmd ]