Extension { #name : 'Rectangle' }

{ #category : '*Morphic-Core' }
Rectangle >> newRectButtonPressedDo: newRectBlock inWorld: aWorld [
	"Track the outline of a new rectangle until mouse button changes. newFrameBlock produces each new rectangle from the previous. Only tracks while mouse is down."
	| rect newRect buttonNow hand |

	hand := aWorld activeHand.

	buttonNow := hand anyButtonPressed.
	rect := self.


	hand captureEventsWhile: [:evt |
		evt isMouse ifTrue: [
			buttonNow := evt anyButtonPressed.
			newRect := newRectBlock value: rect value: evt cursorPoint.
			newRect = rect ifFalse:
				[ rect := newRect ].

			 buttonNow
			] ifFalse: [ true ]
		].

	aWorld activeHand
		newMouseFocus: nil;
		showTemporaryCursor: nil.
	^ rect
]
