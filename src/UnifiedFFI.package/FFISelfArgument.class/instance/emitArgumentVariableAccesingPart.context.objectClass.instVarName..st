private emitting code
emitArgumentVariableAccesingPart: aBuilder context: aContext objectClass: objectClass instVarName: aName

	aBuilder 
		pushReceiver;
		pushLiteral: (objectClass 
			instVarIndexFor: aName 
			ifAbsent: [ self error: 'No ', aName, ' instVar' ])