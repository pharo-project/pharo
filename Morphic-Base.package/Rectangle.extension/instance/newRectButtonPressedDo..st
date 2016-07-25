*Morphic-Base
newRectButtonPressedDo: newRectBlock 
	"Track the outline of a new rectangle until mouse button changes. newFrameBlock produces each new rectangle from the previous. Only tracks while mouse is down."
	| rect newRect buttonNow hand |
	
	hand := World activeHand.
	
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
	World activeHand
		newMouseFocus: nil;
		showTemporaryCursor: nil.
	^ rect