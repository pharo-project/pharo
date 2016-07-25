parsing
parseNamedInstVarsFor: anObject
	self parseMapDo: [ :instVarName :value |
		anObject instVarNamed: instVarName put: value ]