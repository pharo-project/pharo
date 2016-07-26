private emitting code
emitArgumentVariableAccesingPart: aBuilder context: aContext objectClass: objectClass instVarName: aName
	aBuilder 
		pushTemp: self argName asSymbol;
		pushLiteral: (objectClass 
			instVarIndexFor: aName 
			ifAbsent: [ self error: 'No ', aName, ' instVar' ]).