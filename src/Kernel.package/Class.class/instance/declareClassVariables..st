initialize-release
declareClassVariables: newVars 
	"Declare class variables common to all instances. Answer whether 
	recompilation is advisable."
	|  conflicts |
	
	conflicts := false.	
	"Remove. Warn if vars are removed that are still used"
	"self flag: something wrong here. put to true and add an ivar to a class"
	(self classVariables reject: [:x | newVars includes: x]) do: [:var | self removeClassVarNamed: var name interactive: false].
	
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