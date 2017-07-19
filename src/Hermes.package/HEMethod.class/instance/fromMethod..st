instance creation
fromMethod: aMethod
	| literalSpace |
	name := aMethod selector.
	className := aMethod methodClass name.
	header := aMethod header.
	protocol := aMethod protocol.
	literalSpace := ((aMethod numLiterals + 1) * 4).
	
	bytecode := ByteArray new: (aMethod size - literalSpace).
	1 to: bytecode size do:[:i | bytecode at:i put:(aMethod at:(i+literalSpace))].
	
	literals := aMethod literals collect:[:e | e asExportedLiteral ].
	"The last literal is the binding of the method with the holding class, it is easy to reconstruct"
	literals at: (aMethod literals size) put: nil asExportedLiteral.