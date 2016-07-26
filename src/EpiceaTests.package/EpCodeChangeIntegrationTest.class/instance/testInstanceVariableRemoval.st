tests
testInstanceVariableRemoval

	aClass := classFactory 
		newSubclassOf: Object 
		instanceVariableNames: 'x' 
		classVariableNames: ''.

	self assert: (self countLogEventsWith: EpClassModification) equals: 0.
	
	aClass removeInstVarNamed: #x.

	self assert: (self countLogEventsWith: EpClassModification) equals: 1.
	self assert: (self allLogEventsWith: EpClassModification) first hasNonMetaclassChanges