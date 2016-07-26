testing
testPoolVariable
	| poolVar newClass |
	
	poolVar:= RGPoolVariableDefinition named: 'TextConstants'.
	
	self assert: (poolVar isPoolVariable).
	self assert: (poolVar isVariable).
	self assert: (poolVar name = 'TextConstants').
	self assert: (poolVar parent isNil).
	self assert: (poolVar isMetaSide not).
	
	newClass:= RGClassDefinition named: #OrderedCollection.
	poolVar:= (RGPoolVariableDefinition named:  'TextConstants' ) parent: newClass.
	self assert: (poolVar parent = newClass).
	self assert: (newClass sharedPools isEmpty).
	
	newClass sharedPools add: poolVar.
	self assert: (newClass sharedPools size = 1).