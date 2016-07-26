tests
testInstanceVariableAddition

	aClass := classFactory newClass.

	self assert: (self countLogEventsWith: EpClassModification) equals: 0.
		
	aClass addInstVarNamed: #x.

	self assert: (self countLogEventsWith: EpClassModification) equals: 1.
	self assert: (self allLogEventsWith: EpClassModification) first hasNonMetaclassChanges