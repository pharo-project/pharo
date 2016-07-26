constructor
codeTemp: index named: tempName

	^ TempVariableNode new
		name: tempName
		index: index
		type: LdTempType
		scope: 0