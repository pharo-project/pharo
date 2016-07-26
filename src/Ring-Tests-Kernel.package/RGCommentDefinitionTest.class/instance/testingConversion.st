testing
testingConversion
	
	| rgComm |
	rgComm := RGCommentDefinition realClass: RGClassDefinition.
	self assert: rgComm asString equals: 'RGClassDefinition Comment'.
	
	rgComm := RGCommentDefinition class: (RGClassDefinition named: #Foo).
	self assert: rgComm asString equals: 'Foo Comment'