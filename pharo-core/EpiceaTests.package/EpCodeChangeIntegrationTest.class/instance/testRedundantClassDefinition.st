tests
testRedundantClassDefinition

	aClass := classFactory newClass.

	self assert: (self countLogEventsWith: EpClassModification) equals: 0.
		
	self class compiler evaluate: aClass definition.

	self assert: (self countLogEventsWith: EpClassModification) equals: 0.

	self class compiler evaluate: aClass theMetaClass definition.

	self assert: (self countLogEventsWith: EpClassModification) equals: 0.
