private emitting code
emitArgumentVariableAccesingPart: aBuilder context: aContext objectClass: objectClass instVarName: aName
	"Emit access to calling variable (changes depending on the type)."
	self subclassResponsibility