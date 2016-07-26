testing
testClassVariable
	| instVar newClass |
	
	instVar:= RGClassVariableDefinition named: 'DependentsFields'.
	
	self assert: (instVar isClassVariable).
	self assert: (instVar isVariable).
	self assert: (instVar name = 'DependentsFields').
	self assert: (instVar parent isNil).
	self assert: (instVar isMetaSide not).   
	
	newClass:= RGClassDefinition named: #Object.
	instVar:= (RGClassVariableDefinition named: 'DependentsFields') parent: newClass.
	
	self assert: (instVar parent = newClass).
	self assert: (newClass classVariables isEmpty).
	
	newClass classVariables add: instVar.
	self assert: (newClass classVariables size = 1).