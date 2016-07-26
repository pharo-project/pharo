accessing
removeSelf
	"Nil the receiver pointer and answer its former value."

	| tempSelf |
	tempSelf := receiver.
	receiver := nil.
	^tempSelf