private emitting code
emitArgumentVariableAccesingPart: aBuilder context: aContext objectClass: objectClass instVarName: aName
	aBuilder 
		pushInstVar: (aContext methodClass instVarIndexFor: self argName);
		pushLiteral: (objectClass 
			instVarIndexFor: aName 
			ifAbsent: [ self error: 'No ', aName, ' instVar' ]).
