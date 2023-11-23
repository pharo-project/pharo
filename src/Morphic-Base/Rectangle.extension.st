Extension { #name : 'Rectangle' }

{ #category : '*Morphic-Base' }
Rectangle >> asLayoutFrame [
	"Answer an instance of LayoutFrame which uses fractions from receiver "

	^ LayoutFrame new
		leftFraction: self left;
		rightFraction: self right;
		topFraction: self top;
		bottomFraction: self bottom;
		yourself
]

{ #category : '*Morphic-Base' }
Rectangle >> newRectButtonPressedDo: newRectBlock inWorld: aWorld [
	"Track the outline of a new rectangle until mouse button changes. newFrameBlock produces each new rectangle from the previous. Only tracks while mouse is down."
	| rect newRect buttonNow hand |

	hand := aWorld activeHand.

	buttonNow := hand anyButtonPressed.
	rect := self.
	self drawReverseFrame: rect.

	hand captureEventsWhile: [:evt |
		evt isMouse ifTrue: [
			buttonNow := evt anyButtonPressed.
			newRect := newRectBlock value: rect value: evt cursorPoint.
			newRect = rect ifFalse:
				[self drawReverseFrame: rect.
				self drawReverseFrame: newRect.
				rect := newRect ].

			 buttonNow
			] ifFalse: [ true ]
		].

	self drawReverseFrame: rect.
	aWorld activeHand
		newMouseFocus: nil;
		showTemporaryCursor: nil.
	^ rect
]
