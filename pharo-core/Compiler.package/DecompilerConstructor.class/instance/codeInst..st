constructor
codeInst: index

	^InstanceVariableNode new
		name: (instVars at: index + 1 ifAbsent: ['unknown', index asString])
		index: index + 1