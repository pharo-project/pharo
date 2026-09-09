Extension { #name : 'PointerLayout' }

{ #category : '*Shout' }
PointerLayout >> hasBindingThatBeginsWith: aString [
	"Answer true if there is a Slot that begins with aString, false otherwise"

	^ self slotScope hasBindingThatBeginsWith: aString
]
