decompiling
addJumpBackTarget: label to: sequence

	(jumpBackTargetStacks at: label ifAbsentPut: [OrderedCollection new])
		addLast: sequence