tests
"protocol: tests"
testSizeInMemoryLargeInstances
	"Two words for the object header + a word per instance variable"

	self assert: Smalltalk allClasses sizeInMemory = (5 * Smalltalk wordSize).
	self
		assert:
			Smalltalk allClasses asArray sizeInMemory
				= (Smalltalk wordSize * 3 + (Smalltalk allClasses asArray size * Smalltalk wordSize))
