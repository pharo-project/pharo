control
popUpEvent: evt in: aWorld
	"Present this menu in response to the given event."

	| aHand aPosition |
	aHand := evt ifNotNil: [evt hand] ifNil: [ActiveHand].
	aPosition := aHand position truncated.
	^ self popUpAt: aPosition forHand: aHand in: aWorld
