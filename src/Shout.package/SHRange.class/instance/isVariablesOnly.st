tests
isVariablesOnly
	^ self isAssignment
		or: [self isVariable
				or: [self isConstant
						or: [self isSeparator or:[self isBinary]]]]