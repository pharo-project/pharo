emitting code
emitArgument: aBuilder context: aContext objectClass: objectClass instVarName: aName
	"If argument is an object, I need to send value of instVar aName instead it self"
	aBuilder 
		pushTemp: self argName asSymbol;
		pushLiteral: (objectClass 
			instVarIndexFor: aName 
			ifAbsent: [ self error: 'No ', aName, ' instVar' ]);
		send: #instVarAt: