tests
testClassInstanceVariableRemoval

	aClass := classFactory newClass.
	aClass class addInstVarNamed: #x.

	self assert: (self countLogEventsWith: EpClassModification) equals: 1.
		
	aClass class removeInstVarNamed: #x.

	self assert: (self countLogEventsWith: EpClassModification) equals: 2.
	self assert: ((self allLogEventsWith: EpClassModification) allSatisfy: #hasMetaclassChanges)