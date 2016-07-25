instance creation
fromHandle: aHandle type: aType
	"Create a pointer to the given type"
	"ExternalData fromHandle: ExternalAddress new type: ExternalType float"
	^self basicNew setHandle: aHandle type: aType