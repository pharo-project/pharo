tests
testSizeInMemoryLargeInstances
	"Two words for the object header + a world per instance variable"

	self assert: Smalltalk allClasses sizeInMemory = 20.
	self
		assert:
			Smalltalk allClasses asArray sizeInMemory
				= (Smalltalk wordSize * 3 + (Smalltalk allClasses asArray size * Smalltalk wordSize))