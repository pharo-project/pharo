testing
testInstanceVariable
	| instVar newClass |
	
	instVar:= RGInstanceVariableDefinition named: 'size'.
	
	self assert: (instVar isInstanceVariable).
	self assert: (instVar isVariable).
	self assert: (instVar name = 'size').
	self assert: (instVar parent isNil).
	self assert: (instVar isMetaSide not).
	
	newClass:= RGClassDefinition named: #OrderedCollection.
	instVar:= newClass addInstVarNamed: 'array'.
	self assert: (instVar parent = newClass).
	self assert: (newClass instanceVariables size = 1).