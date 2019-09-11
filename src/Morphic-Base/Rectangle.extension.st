Extension { #name : #Rectangle }

{ #category : #'*Morphic-Base' }
Rectangle class >> activeHand [

	^ ActiveHand
]

{ #category : #'*Morphic-Base' }
Rectangle >> asLayoutFrame [
	"Answer an instance of LayoutFrame which uses fractions from receiver "
	
	^ LayoutFrame new
		leftFraction: self left;
		rightFraction: self right;
		topFraction: self top;
		bottomFraction: self bottom;
		yourself
		
]

{ #category : #'*Morphic-Base' }
Rectangle >> drawReverseFrame: rect [

	Display 
		border: rect
		width: 2
		rule: Form reverse
		fillColor: Color gray
]

{ #category : #'*Morphic-Base' }
Rectangle class >> fromUser [
	"Answer an instance of me that is determined by having the user 
	designate the top left and bottom right corners. The gridding for user 
	selection is 1@1."

	^self fromUser: 1 @ 1
]

{ #category : #'*Morphic-Base' }
Rectangle class >> fromUser: gridPoint [ 
	"Answer a Rectangle that is determined by having the user 
	designate the top left and bottom right corners. 
	The cursor reamins linked with the sensor, but
	the outline is kept gridded."
	| originRect |
	self activeHand showTemporaryCursor: Cursor origin.

	originRect :=  ((self activeHand cursorPoint grid: gridPoint) extent: 0 @ 0) 
					newRectFrom: [ :f :pt | (pt grid: gridPoint) extent: 0 @ 0 ].
					
	self activeHand showTemporaryCursor: Cursor bottomRight.

	^  originRect newRectFrom: [ :f :pt | f origin corner: (pt grid: gridPoint) ]
]

{ #category : #'*Morphic-Base' }
Rectangle >> newRectButtonPressedDo: newRectBlock [ 
	"Track the outline of a new rectangle until mouse button changes. newFrameBlock produces each new rectangle from the previous. Only tracks while mouse is down."
	| rect newRect buttonNow hand |
	
	hand := self currentWorld activeHand.
	
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
	self currentWorld activeHand
		newMouseFocus: nil;
		showTemporaryCursor: nil.
	^ rect
]

{ #category : #'*Morphic-Base' }
Rectangle >> newRectFrom: newRectBlock [ 
	"Track the outline of a new rectangle until mouse button changes. newFrameBlock produces each new rectangle from the previous"
	| rect newRect buttonStart buttonNow  hand |

	hand := self currentWorld activeHand.
	
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
	

]
