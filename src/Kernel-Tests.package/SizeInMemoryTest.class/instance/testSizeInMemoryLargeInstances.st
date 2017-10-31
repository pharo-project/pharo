tests
testSizeInMemoryLargeInstances

	"64 bits for the object header + a world per instance variable"

	self
		assert: Smalltalk allClasses sizeInMemory = (self align64Bits: (self headerSize + (3 * Smalltalk wordSize)));
		assert:
			Smalltalk allClasses asArray sizeInMemory
				=
					(self align64Bits:(self headerSize + (Smalltalk allClasses asArray size * Smalltalk wordSize)
						+ self headerSize))