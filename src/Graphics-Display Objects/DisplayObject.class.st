"
The abstract protocol for most display primitives that are used by Views for presenting information on the screen.
"
Class {
	#name : #DisplayObject,
	#superclass : #Object,
	#category : #'Graphics-Display Objects'
}

{ #category : #transforming }
DisplayObject >> align: alignmentPoint with: relativePoint [ 
	"Translate the receiver's offset such that alignmentPoint aligns with 
	relativePoint."

	self offset: (self offset translateBy: relativePoint - alignmentPoint)
]

{ #category : #'display box access' }
DisplayObject >> boundingBox [
	"Answer the rectangular area that represents the boundaries of the 
	receiver's space of information."

	^self computeBoundingBox
]

{ #category : #'display box access' }
DisplayObject >> center [

	^ self boundingBox center
]

{ #category : #'display box access' }
DisplayObject >> computeBoundingBox [
	"Answer the rectangular area that represents the boundaries of the 
	receiver's area for displaying information. This is the primitive for 
	computing the area if it is not already known."

	self subclassResponsibility
]

{ #category : #'displaying-display' }
DisplayObject >> display [ 
	"Display the receiver on the Display at location 0,0."

	self displayOn: Display
]

{ #category : #'displaying-generic' }
DisplayObject >> displayAt: aDisplayPoint [ 
	"Display the receiver located at aDisplayPoint with default settings for 
	the displayMedium, rule and halftone."

	self displayOn: Display
		at: aDisplayPoint
		clippingBox: Display boundingBox
		rule: Form over
		fillColor: nil
]

{ #category : #'displaying-generic' }
DisplayObject >> displayOn: aDisplayMedium [
	"Simple default display in order to see the receiver in the upper left 
	corner of screen."

	self displayOn: aDisplayMedium at: 0 @ 0
]

{ #category : #'displaying-generic' }
DisplayObject >> displayOn: aDisplayMedium at: aDisplayPoint [ 
	"Display the receiver located at aDisplayPoint with default settings for 
	rule and halftone."

	self displayOn: aDisplayMedium
		at: aDisplayPoint
		clippingBox: aDisplayMedium boundingBox
		rule: Form over
		fillColor: nil
]

{ #category : #'displaying-generic' }
DisplayObject >> displayOn: aDisplayMedium at: aDisplayPoint clippingBox: clipRectangle [ 
	"Display the receiver located at aDisplayPoint with default settings for 
	rule and halftone. Information to be displayed must be confined to the 
	area that intersects with clipRectangle."

	self displayOn: aDisplayMedium
		at: aDisplayPoint
		clippingBox: clipRectangle
		rule: Form over
		fillColor: nil
]

{ #category : #'displaying-generic' }
DisplayObject >> displayOn: aDisplayMedium at: aDisplayPoint clippingBox: clipRectangle rule: ruleInteger fillColor: aForm [
	"This is the basic display primitive for graphic display objects. Display 
	the receiver located at aDisplayPoint with rule, ruleInteger, and mask, 
	aForm. Information to be displayed must be confined to the area that 
	intersects with clipRectangle."

	self subclassResponsibility
]

{ #category : #'displaying-generic' }
DisplayObject >> displayOn: aDisplayMedium at: aDisplayPoint rule: ruleInteger [
	"Display the receiver located at aPoint with default setting for the 
	halftone and clippingBox."

	self displayOn: aDisplayMedium
		at: aDisplayPoint
		clippingBox: aDisplayMedium boundingBox
		rule: ruleInteger
		fillColor: nil
]

{ #category : #'displaying-generic' }
DisplayObject >> displayOnPort: port at: location rule: rule [

	port copyForm: self to: location rule: rule.

]

{ #category : #accessing }
DisplayObject >> extent [
	"Answer the point that represents the width and height of the receiver's 
	bounding box."

	^self boundingBox extent
]

{ #category : #'displaying-display' }
DisplayObject >> follow: locationBlock while: durationBlock [
   "Move an image around on the Display. Restore the background
   continuously without causing flashing. The argument, locationBlock,
   supplies each new location, and the argument, durationBlock, supplies
   true to continue, and then false to stop."
 
   | bitsBehind loc |
	loc := locationBlock value.
   bitsBehind := Form fromDisplay: (loc extent: self extent).
   ^ self follow: locationBlock while: durationBlock bitsBehind: bitsBehind startingLoc: loc
]

{ #category : #'displaying-display' }
DisplayObject >> follow: locationBlock while: durationBlock bitsBehind: initialBitsBehind startingLoc: loc [
   "Move an image around on the Display. Restore the background continuously without causing flashing. The argument, locationBlock, supplies each new location, and the argument, durationBlock, supplies true to continue or false to stop. This variant takes the bitsBehind as an input argument, and returns the final saved saved bits as method value."

   | location rect1 save1 save1Blt buffer bufferBlt newLoc rect2 bothRects |
   location := loc.
   rect1 := location extent: self extent.
   save1 := initialBitsBehind.
   save1Blt := BitBlt toForm: save1.
   buffer := Form extent: self extent*2 depth: Display depth.  "Holds overlapping region"
   bufferBlt := BitBlt toForm: buffer.

   Display deferUpdates: true.
   self displayOn: Display at: location rule: Form paint.
   Display deferUpdates: false; forceToScreen: (location extent: self extent).

   [durationBlock value] whileTrue: [
		newLoc := locationBlock value.
		newLoc ~= location ifTrue: [
			rect2 := newLoc extent: self extent.
			bothRects := rect1 merge: rect2.
			(rect1 intersects: rect2)
				ifTrue: [  "when overlap, buffer background for both rectangles"
					bufferBlt copyFrom: bothRects in: Display to: 0@0.
					bufferBlt copyFrom: save1 boundingBox in: save1 to: rect1 origin - bothRects origin.
					"now buffer is clean background; get new bits for save1"
					save1Blt copy: (0@0 extent: self extent) from: rect2 origin - bothRects origin in: buffer.
					self displayOnPort: bufferBlt at: rect2 origin - bothRects origin rule: Form paint.
					Display deferUpdates: true.
					Display copy: bothRects from: 0@0 in: buffer rule: Form over.
					Display deferUpdates: false; forceToScreen: bothRects]
				ifFalse: [  "when no overlap, do the simple thing (both rects might be too big)"
					Display deferUpdates: true.
					Display copy: (location extent: save1 extent) from: 0@0 in: save1 rule: Form over.
					save1Blt copyFrom: rect2 in: Display to: 0@0.
					self displayOn: Display at: newLoc rule: Form paint.
					Display deferUpdates: false; 
						forceToScreen: (location extent: save1 extent); 
						forceToScreen: (newLoc extent: self extent)].
			location := newLoc.
			rect1 := rect2]].

	^ save1 displayOn: Display at: location

]

{ #category : #accessing }
DisplayObject >> height [
	"Answer the number that represents the height of the receiver's 
	bounding box."

	^self boundingBox height
]

{ #category : #'display box access' }
DisplayObject >> initialExtent [
	"Included here for when a FormView is being opened
	as a window.  (4@4) covers border widths."

	^ self extent + (4@4) 
]

{ #category : #'displaying-display' }
DisplayObject >> isTransparent [
	^ false
]

{ #category : #accessing }
DisplayObject >> offset [
	"Answer the amount by which the receiver should be offset when it is 
	displayed or its position is tested."

	self subclassResponsibility
]

{ #category : #accessing }
DisplayObject >> offset: aPoint [ 
	"Set the amount by which the receiver's position is offset."

	^self
]

{ #category : #accessing }
DisplayObject >> relativeRectangle [
	"Answer a Rectangle whose top left corner is the receiver's offset position 
	and whose width and height are the same as the receiver."

	^Rectangle origin: self offset extent: self extent
]

{ #category : #'truncation and round off' }
DisplayObject >> rounded [
	"Convert the offset of the receiver to integer coordinates."

	self offset: self offset rounded
]

{ #category : #transforming }
DisplayObject >> scaleBy: aPoint [ 
	"Scale the receiver's offset by aPoint."

	self offset: (self offset scaleBy: aPoint)
]

{ #category : #'displaying-display' }
DisplayObject >> slideFrom: startPoint to: stopPoint nSteps: nSteps delay: milliSecs [
	"Slide this object across the display over the given number of steps, pausing for the given number of milliseconds after each step."
	"Note: Does not display at the first point, but does at the last."

	| i p delta |
	i := 0.
	p := startPoint.
	delta := (stopPoint - startPoint) / nSteps asFloat.
	^ self
		follow: [
			p := p + delta.
			p truncated]
		while: [
			(Delay forMilliseconds: milliSecs) wait.
			(i := i + 1) < nSteps]

]

{ #category : #'displaying-display' }
DisplayObject >> slideWithFirstFrom: startPoint to: stopPoint nSteps: nSteps delay: milliSecs [ 
	"Slide this object across the display over the given number of steps, 
	pausing for the given number of milliseconds after each step."
	"Note: Does display at the first point and at the last."
	| i p delta |
	i := 0.
	delta := stopPoint - startPoint / nSteps asFloat.
	p := startPoint - delta.
	^ self 
		follow: [(p := p + delta) truncated]
		while: 
			[(Delay forMilliseconds: milliSecs) wait.
			(i := i + 1) <= nSteps]
]

{ #category : #transforming }
DisplayObject >> translateBy: aPoint [ 
	"Translate the receiver's offset."

	self offset: (self offset translateBy: aPoint)
]

{ #category : #accessing }
DisplayObject >> width [
	"Answer the number that represents the width of the receiver's bounding 
	box."

	^self boundingBox width
]
