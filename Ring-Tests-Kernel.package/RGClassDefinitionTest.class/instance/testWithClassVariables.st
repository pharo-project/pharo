testing
testWithClassVariables
	| newClass classVar |
	
	newClass:= (RGClassDefinition named: #Object)
						addClassVariables: #(DependentsFields);
						addClassVarNamed: #FakeVariable;
						yourself.

	self assert: (newClass classVariables size = 2).
	self assert: (newClass classVarNames size = 2).
	self assert: (newClass allClassVarNames size = 2).  "no hierarchy"
	
	classVar:= newClass classVarNamed: #DependentsFields.
	self assert: (classVar notNil).
	self assert: (classVar isClassVariable).
	self assert: (classVar isVariable).
	self assert: (classVar parent = newClass).
	self assert: (classVar parentName == newClass name).
	self assert: (classVar realClass = Object).
	
	newClass withMetaclass.
	self assert: (newClass theMetaClass allClassVarNames size = 2).
	
	newClass removeClassVarNamed: #DependentsFields.
	self assert: (newClass classVariables size = 1).