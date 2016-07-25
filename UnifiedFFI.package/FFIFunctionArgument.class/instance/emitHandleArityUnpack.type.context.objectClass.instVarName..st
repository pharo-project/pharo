emitting code
emitHandleArityUnpack: aBuilder type: aType context: aContext objectClass: objectClass instVarName: aName
	"This a special kind of arity-unpacking, that works with FFIConstantHandleType"

	aType needsArityUnpacking ifFalse: [ ^ self ].
	
	"Push accessing variable (this change depending on the argument type)"
	self 
		emitArgumentVariableAccesingPart: aBuilder 
		context: aContext 
		objectClass: objectClass 
		instVarName: aName.
	"Unpack argument"
	aBuilder
		pushTemp: self rolledPointerTempName;
		pushLiteral: aType pointerArity;
		send: #unpackHandleFromArity:.
	"Send assignment"
	aBuilder send: #instVarAt:put:.
	"... and return top"
	aBuilder popTop.