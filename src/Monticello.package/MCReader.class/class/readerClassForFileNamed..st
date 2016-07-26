testing
readerClassForFileNamed: fileName
	^ self concreteSubclasses
		detect: [:c | c canReadFileNamed: fileName]
		ifNone: [nil]