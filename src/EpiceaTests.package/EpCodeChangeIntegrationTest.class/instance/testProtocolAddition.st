tests
testProtocolAddition

	aClass := classFactory newClass.
	aClass organization addCategory: #testing42.

	self assert: (self countLogEventsWith: EpProtocolAddition) equals: 1