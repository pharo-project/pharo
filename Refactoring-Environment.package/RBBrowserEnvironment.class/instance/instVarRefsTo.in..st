environments
instVarRefsTo: instVarName in: aClass 
	^RBVariableEnvironment 
		on: self
		referencesToInstanceVariable: instVarName
		in: aClass