instructions
pushLiteral: object
	stack push.
	"identity includes else 0.0 = 0 whereas it's a Float" 
	(self encoderClass specialLiterals identityIncludes: object) ifTrue: [ 
		^ encoder genPushSpecialLiteral: object ].
	encoder genPushLiteral: (self literalIndexOf: object)
