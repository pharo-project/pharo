initialize-release
abstractVariablesIn: aBRProgramNode from: fromBehavior toAll: behaviorCollection ignoring: aVariableName 
	| poolRefactoring |
	tree := aBRProgramNode.
	fromClass := self classObjectFor: fromBehavior.
	toClasses := behaviorCollection 
				collect: [:each | self classObjectFor: each].
	ignore := aVariableName.
	poolRefactoring := RBExpandReferencedPoolsRefactoring 
				model: self model
				forMethod: tree
				fromClass: fromClass
				toClasses: toClasses.
	self performComponentRefactoring: poolRefactoring.
	self computeVariablesToAbstract