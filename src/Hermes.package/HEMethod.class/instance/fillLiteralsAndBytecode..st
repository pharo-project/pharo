instance creation
fillLiteralsAndBytecode: aMethod
	| literalSpace |
	
	encoderClassName := aMethod encoderClass name.
	numArgs := aMethod numArgs.
	numTemps := aMethod numTemps.
	hasPrimitive := aMethod isPrimitive asBit.

	literalSpace := (aMethod numLiterals + 1) * Smalltalk wordSize.

	bytecode := ByteArray new: aMethod size - literalSpace.
	1 to: bytecode size do: [ :i | bytecode at: i put: (aMethod at: i + literalSpace) ].

	literals := aMethod literals allButLast collect: [ :e | e asExportedLiteral ].
	"The last literal is the binding of the method with the holding class, it is easy to reconstruct"
	literals := literals copyWith: nil asExportedLiteral.