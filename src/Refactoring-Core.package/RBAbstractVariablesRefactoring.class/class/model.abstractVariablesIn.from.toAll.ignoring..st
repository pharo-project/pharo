instance creation
model: aRBSmalltalk abstractVariablesIn: aBRProgramNode from: fromBehavior toAll: behaviorCollection ignoring: aVariableName 
	^(self new)
		model: aRBSmalltalk;
		abstractVariablesIn: aBRProgramNode
			from: fromBehavior
			toAll: behaviorCollection
			ignoring: aVariableName;
		yourself