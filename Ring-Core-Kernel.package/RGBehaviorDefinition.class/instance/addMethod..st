adding/removing methods
addMethod: aRGMethodDefinition
	"Adds aRGMethodDefinition in the methods dictionary.
	 Adds the protocol of such method too"

	self addMethod: aRGMethodDefinition in: methods.
	self addProtocol: aRGMethodDefinition protocol