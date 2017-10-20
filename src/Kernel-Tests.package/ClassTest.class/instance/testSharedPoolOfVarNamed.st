tests - access
testSharedPoolOfVarNamed
	"self debug: #testSharedPoolOfVarNamed"

	self assert: (Date sharedPoolOfVarNamed: 'DayNames') = ChronologyConstants.
	"a metaclass does not have shared pools since only classes have shared pools"
	self assert: (Date class sharedPoolOfVarNamed: 'DayNames') isNil.
	
	self assert: (RootClassPoolUser sharedPoolOfVarNamed: 'Author') = PoolDefiner.
	self assert: (RootClassPoolUser sharedPoolOfVarNamed: 'Gloups') = PoolDefiner.
	self assert: (SubclassPoolUser sharedPoolOfVarNamed: 'Author') = PoolDefiner.
	
	self assert: (ClassMultiplePoolUser sharedPoolOfVarNamed: 'Author') = PoolDefiner.
	self assert: (ClassMultiplePoolUser sharedPoolOfVarNamed: 'VariableInPoolDefiner2') = PoolDefiner2.
	self assert: (ClassMultiplePoolUser sharedPoolOfVarNamed: 'Gloups') = PoolDefiner.