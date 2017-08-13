tests
testMetaclassInstanceVariableAddition

	aClass := classFactory newClass.

	self assert: (self countLogEventsWith: EpClassModification) equals: 0.
	aClass theMetaClass instanceVariableNames: 'x'.	
	self assert: (self countLogEventsWith: EpClassModification) equals: 1.
