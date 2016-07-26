write and read
nextPut: anObject
	"Write anObject to the receiver stream. Answer anObject."
	| typeID selector objectToStore |

	typeID := self typeIDFor: anObject.
	(self tryToPutReference: anObject typeID: typeID)
		ifTrue: [^ anObject].

	objectToStore := (self objectIfBlocked: anObject).
	objectToStore == anObject ifFalse: [typeID := self typeIDFor: objectToStore].

	byteStream nextPut: typeID.
	selector := self class writeSelectors at: typeID.
	self perform: selector with: objectToStore.

	^ anObject