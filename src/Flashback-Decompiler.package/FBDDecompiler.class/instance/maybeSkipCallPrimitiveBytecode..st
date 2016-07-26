private
maybeSkipCallPrimitiveBytecode: aCompiledMethod
	"If the method's bytecode set uses the callPrimitive bytecode, ignore it.
	The primitive is handled specifically before bytecode interpretation."
	instructionStream nextByte = aCompiledMethod encoderClass callPrimitiveCode 
		ifTrue: [ instructionStream nextInstruction ]