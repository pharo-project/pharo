environments
instVarReadersTo: instVarName in: aClass 
	^RBVariableEnvironment 
		on: self
		readersOfInstanceVariable: instVarName
		in: aClass