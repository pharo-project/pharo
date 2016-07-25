environments
classVarRefsTo: instVarName in: aClass 
	^ RBVariableEnvironment 
		on: self
		referencesToClassVariable: instVarName
		in: aClass