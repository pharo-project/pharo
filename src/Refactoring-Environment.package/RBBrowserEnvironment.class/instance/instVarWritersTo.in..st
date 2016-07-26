environments
instVarWritersTo: instVarName in: aClass 
	^RBVariableEnvironment 
		on: self
		writersOfInstanceVariable: instVarName
		in: aClass