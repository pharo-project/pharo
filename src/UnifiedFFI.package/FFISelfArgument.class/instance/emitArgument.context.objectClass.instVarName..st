emitting code
emitArgument: aBuilder context: aContext objectClass: objectClass instVarName: aName
	"If argument is an object, I need to send value of instVar handle instead it self"
	aBuilder pushInstVar: (objectClass 
		instVarIndexFor: aName 
		ifAbsent: [ self error: 'No ', aName, ' instVar' ])