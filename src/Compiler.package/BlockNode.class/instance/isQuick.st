testing
isQuick
	^ statements size = 1
		and: [statements first isVariableReference
				or: [statements first isSpecialConstant]]