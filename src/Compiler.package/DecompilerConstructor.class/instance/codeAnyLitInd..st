constructor
codeAnyLitInd: association

	^VariableNode new
		name: association key
		key: association
		index: 0
		type: LdLitIndType