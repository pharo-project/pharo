initialize-release
declareClassVariables: newVars 
	"Declare class variables common to all instances. Answer whether 
	recompilation is advisable."
	|  conflicts |
	
	conflicts := false.	
	(self classVariables reject: [:x | newVars includes: x]) do: [:var | [self removeClassVariable: var] on: InMidstOfFileinNotification do: [:ex | ex resume: true]].
	
	(newVars reject: [:var | self classVariables includes: var]) do: [:var | "adding"
			"check if new vars defined elsewhere"
			(self innerBindingOf: var key) 
				ifNotNil: [(DuplicatedVariableError new) variable: var name;
						signal: var name , ' is defined elsewhere'.
					conflicts := true]].
	newVars notEmpty
		ifTrue: 
			[self classPool: self classPool.
			"in case it was nil"
			newVars do: [:var | self classPool declareVariable: var from: Undeclared]].
	^conflicts