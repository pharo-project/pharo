guinea pigs
maybeReadOnlyObjects
	"ByteObject, Variable object, fixed sized object"
	^ { { 1 . 2 . 3 } asByteArray . { 1 . 2 . 3 } . (MessageSend receiver: 1 selector: #+ argument: 2) }