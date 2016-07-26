tests
testClassInstanceVariableAddition

	aClass := classFactory newClass.

	self assert: (self countLogEventsWith: EpClassModification) equals: 0.
		
	aClass class addInstVarNamed: #x.

	self assert: (self countLogEventsWith: EpClassModification) equals: 1.
	self assert: (self allLogEventsWith: EpClassModification) first hasMetaclassChanges