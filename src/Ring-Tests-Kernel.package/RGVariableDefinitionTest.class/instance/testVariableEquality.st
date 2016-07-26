testing
testVariableEquality

	| variable1 variable2|
	
	variable1 := (OrderedCollection asRingDefinition) instanceVariableNamed: #array.
	variable2 := (OrderedCollection asRingDefinition) instanceVariableNamed: #array.
	self assert: variable1 = variable2.

	variable1 := (RGClassVariableDefinition named:  #Foo) parentName: #Test.
	variable2 := (RGClassVariableDefinition named:  #Foo) parentName: #Test.
	self assert: variable1 = variable2.
	
	variable2 := (RGClassVariableDefinition named:  #Foo) parentName: #Test2.
	self deny: variable1 = variable2.
	