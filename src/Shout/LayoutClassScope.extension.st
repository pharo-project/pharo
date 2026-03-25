Extension { #name : 'LayoutClassScope' }

{ #category : '*Shout' }
LayoutClassScope >> hasBindingThatBeginsWith: aString [
	"Answer true if there is a Slot name that begins with aString, false otherwise"

	self allSlotsDo: [:each |
		(each name beginsWith: aString)
			ifTrue:[^true]].
	^false
]
