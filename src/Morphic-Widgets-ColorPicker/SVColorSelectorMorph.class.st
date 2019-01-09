"
A colour selector that displays an area with saturation on the x axis and volume on the y axis. Provides interactive selection of colour by mouse. For the moment it is event rather than model based.
Setting the color will specify the hue and setting the selectedColor will specify the saturation and volume (may have a different hue to that displayed if not in sync).
"
Class {
	#name : #SVColorSelectorMorph,
	#superclass : #Morph,
	#instVars : [
		'selectedColor',
		'locationMorph'
	],
	#category : #'Morphic-Widgets-ColorPicker'
}

{ #category : #accessing }
SVColorSelectorMorph >> adoptPaneColor: paneColor [
	"Pass on to the border too."
	
	super adoptPaneColor: paneColor.
	self borderStyle baseColor: paneColor twiceDarker
]

{ #category : #accessing }
SVColorSelectorMorph >> basicColor: aColor [
	"Set the gradient colors."
	
	super color: aColor beOpaque.
	self
		fillStyle: self gradient
]

{ #category : #accessing }
SVColorSelectorMorph >> blackGradient [
	"Answer the black gradient. Top to bottom, transparent to black."

	^(InterpolatedGradientFillStyle colors: {Color black alpha: 0. Color black})
		origin: self innerBounds topLeft;
		direction: 0@self innerBounds height
]

{ #category : #accessing }
SVColorSelectorMorph >> blackGradientMorph [
	"Answer the black gradient morph."

	^Morph new
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		fillStyle: self blackGradient
]

{ #category : #accessing }
SVColorSelectorMorph >> color: aColor [
	"Set the gradient colors."
	
	self
		basicColor: aColor;
		selectedColor: (Color h: aColor hue s: self selectedColor saturation v: self selectedColor brightness)
]

{ #category : #'pixel access' }
SVColorSelectorMorph >> colorAt: aPoint [
	"Answer the color in the world at the given point."
	
	^self isInWorld
		ifTrue: [(Display colorAt: aPoint) beOpaque ]
		ifFalse: [Color black]
]

{ #category : #geometry }
SVColorSelectorMorph >> extent: p [
	"Update the gradient directions."

	super extent: p.
	self updateGradients
]

{ #category : #'visual properties' }
SVColorSelectorMorph >> fillStyle: fillStyle [
	"If it is a color then override with gradient."
	
	fillStyle isColor
		ifTrue: [self color: fillStyle]
		ifFalse: [super fillStyle: fillStyle]
]

{ #category : #accessing }
SVColorSelectorMorph >> gradient [
	"Answer the base gradient."

	|b|
	b := self innerBounds.
	^(GradientFillStyle colors: {Color white. self color})
		origin: b topLeft;
		direction: (b width@0)
]

{ #category : #'event handling' }
SVColorSelectorMorph >> handlesMouseDown: evt [
	"Yes for down and move.." 

	^true
]

{ #category : #'event handling' }
SVColorSelectorMorph >> handlesMouseOverDragging: evt [
	"Yes, make the location morph visible when leaving."
	
	^true
]

{ #category : #displaying }
SVColorSelectorMorph >> hideLocation [
	"Hide the location morph and update the display."

	self locationMorph visible: false; changed
]

{ #category : #initialization }
SVColorSelectorMorph >> initialize [
	"Initialize the receiver."

	super initialize.
	self locationMorph: self newLocationMorph.
	self
		clipSubmorphs: true;
		color: Color blue;
		borderStyle: (BorderStyle inset width: 1);
		addMorphBack: self locationMorph;
		addMorphBack: self blackGradientMorph
]

{ #category : #layout }
SVColorSelectorMorph >> layoutBounds: aRectangle [
	"Set the bounds for laying out children of the receiver.
	Note: written so that #layoutBounds can be changed without touching this method"
	
	super layoutBounds: aRectangle.
	self updateGradients
]

{ #category : #accessing }
SVColorSelectorMorph >> locationMorph [
	"Answer the value of locationMorph"

	^ locationMorph
]

{ #category : #accessing }
SVColorSelectorMorph >> locationMorph: anObject [
	"Set the value of locationMorph"

	locationMorph := anObject
]

{ #category : #'event handling' }
SVColorSelectorMorph >> mouseDown: evt [ 
	"Handle a mouse down event. Select the color at the mouse position."
	
	evt redButtonPressed
		ifFalse: [^super mouseDown: evt].
	evt hand showTemporaryCursor: (Cursor crossHair copy offset: -9 @ -9).
	self hideLocation.
	self selectColorAt: evt position.
	^super mouseDown: evt
]

{ #category : #'event handling' }
SVColorSelectorMorph >> mouseEnterDragging: evt [
	"Make the location morph invisible when entering."
	
	self hideLocation.
	evt hand showTemporaryCursor: (Cursor crossHair copy offset: -9 @ -9).
]

{ #category : #'event handling' }
SVColorSelectorMorph >> mouseLeaveDragging: evt [
	"Make the location morph visible when leaving."
	
	evt hand showTemporaryCursor: nil.
	self showLocation
]

{ #category : #'event handling' }
SVColorSelectorMorph >> mouseMove: evt [ 
	"Handle a mouse move event. Select the color at the mouse position."
	
	evt redButtonPressed
		ifFalse: [^super mouseMove: evt].
	self selectColorAt: evt position.
	^super mouseMove: evt
]

{ #category : #'event handling' }
SVColorSelectorMorph >> mouseUp: evt [ 
	"Handle a up event. Show the location morph again."
	
	evt hand showTemporaryCursor: nil.
	self updateSelectedLocation.
	self locationMorph visible: true
]

{ #category : #'instance creation' }
SVColorSelectorMorph >> newLocationMorph [
	"Answer a new morph indicating the location of the selected color."

	^ImageMorph new
		form: Cursor crossHair withMask asCursorForm
]

{ #category : #accessing }
SVColorSelectorMorph >> selectColorAt: aPoint [
	"Set the color at the given position."
	
	|b p|
	b := self innerBounds.
	p := (b containsPoint: aPoint)
		ifTrue: [aPoint]
		ifFalse: [b pointNearestTo: aPoint].
	p := p - b topLeft / b extent.
	self selectedColor: (Color
		h: self color hue
		s: p x
		v: 1.0 - p y)
]

{ #category : #accessing }
SVColorSelectorMorph >> selectedColor [
	"Answer the value of selectedColor"

	^selectedColor ifNil: [self color]
]

{ #category : #accessing }
SVColorSelectorMorph >> selectedColor: aColor [
	"Set the value of selectedColor."

	selectedColor := aColor.
	self locationMorph visible ifTrue: [self updateSelectedLocation].
	self triggerEvent: #colorSelected with: aColor
]

{ #category : #accessing }
SVColorSelectorMorph >> selectedLocation [
	"Answer the location within the receiver of the selected colour
	relative to the receiver's top left."

	|b c x y|
	b := self innerBounds.
	c := self selectedColor.
	x := c saturation * (b width - 1).
	y := 1 - c brightness * (b height - 1).
	^(x truncated @ y truncated) + b topLeft
]

{ #category : #displaying }
SVColorSelectorMorph >> showLocation [
	"Show the location morph and update the display."
	
	self locationMorph visible: true; changed.
]

{ #category : #updating }
SVColorSelectorMorph >> updateGradients [
	"Update the gradient directions."

	|bgm b|
	b := self innerBounds.
	bgm := self submorphs last.
	bgm bounds: b.
	bgm fillStyle
		origin: b topLeft;
		direction: 0@b height.
	self fillStyle
		origin: b topLeft;
		direction: (b width@0).
	self updateSelectedLocation
]

{ #category : #updating }
SVColorSelectorMorph >> updateSelectedLocation [
	"Position the location morph to indicate the selected colour."
	
	self locationMorph
		position: (self selectedLocation - (self locationMorph extent // 2 + (self locationMorph extent \\ 2)))
]
