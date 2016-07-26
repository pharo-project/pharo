accessing methods
methodsInProtocol: aString
	"Retrieves the methods classified in protocol named aString"

	^methods select: [ :each | each protocol = aString ]