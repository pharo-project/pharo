*Morphic-Base
newRectFrom: newRectBlock 
	"Track the outline of a new rectangle until mouse button changes. newFrameBlock produces each new rectangle from the previous"
	| rect newRect buttonStart buttonNow  hand |

	hand := World activeHand.
	
	buttonStart := buttonNow := hand anyButtonPressed.
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

			 buttonNow = buttonStart
			] ifFalse: [ true ]
		].
	
	self drawReverseFrame:  rect.
	
	hand
		newMouseFocus: nil;
		showTemporaryCursor: nil.
	^ rect
	
