testing
testSorting

	| rgComm1 rgComm2 |
	rgComm1 := RGCommentDefinition realClass: RGClassDefinition.
	rgComm2 := RGCommentDefinition realClass: RGElementDefinition.
	self assert: rgComm1 <= rgComm2