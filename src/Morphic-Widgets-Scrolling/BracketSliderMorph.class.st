"
Abstract superclass for morphs that are used to select a component (R, G, B or A) of a colour.
"
Class {
	#name : #BracketSliderMorph,
	#superclass : #PluggableSliderMorph,
	#category : #'Morphic-Widgets-Scrolling'
}

{ #category : #protocol }
BracketSliderMorph >> defaultFillStyle [
	"Answer the defauolt fill style."

	^Color gray
]

{ #category : #geometry }
BracketSliderMorph >> extent: aPoint [
	"Update the gradient directions."

	super extent: aPoint.
	self updateFillStyle
]

{ #category : #accessing }
BracketSliderMorph >> fillStyleToUse [
	"Answer the fillStyle that should be used for the receiver."
	
	^self fillStyle
]

{ #category : #accessing }
BracketSliderMorph >> gradient [
	"Answer the gradient."

	self subclassResponsibility
]

{ #category : #initialization }
BracketSliderMorph >> initialize [
	"Initialize the receiver."

	super initialize.
	self
		fillStyle: self defaultFillStyle;
		borderStyle: (BorderStyle inset baseColor: self paneColor; width: 1);
		sliderColor: Color black;
		clipSubmorphs: true
]

{ #category : #initialization }
BracketSliderMorph >> initializeSlider [
	"Make the slider raised."
	
	self 
		setSlider: (( BracketMorph newBounds: self totalSliderArea)
						horizontal: self bounds isWide;
						color: self thumbColor;
						borderStyle: (BorderStyle raised baseColor: Color white; width: 1))
		 withShadow: ((BracketMorph newBounds: self totalSliderArea)
						horizontal: self bounds isWide;
						color: self pagingArea color;
						borderStyle: (BorderStyle inset baseColor: (Color white alpha: 0.6); width: 1)).
]

{ #category : #layout }
BracketSliderMorph >> layoutBounds: aRectangle [
	"Set the bounds for laying out children of the receiver.
	Note: written so that #layoutBounds can be changed without touching this method"
	
	super layoutBounds: aRectangle.
	self updateFillStyle.
	slider horizontal: self bounds isWide.
	sliderShadow horizontal: self bounds isWide
]

{ #category : #geometry }
BracketSliderMorph >> roomToMove [
	"Allow to run off the edges a bit."
	
	^self bounds isWide
		ifTrue: [self totalSliderArea insetBy: ((self sliderThickness // 2@0) negated corner: (self sliderThickness // 2 + 1)@0)]
		ifFalse: [self totalSliderArea insetBy: (0@(self sliderThickness // 2) negated corner: 0@(self sliderThickness // 2 - (self sliderThickness \\ 2) + 1))]
]

{ #category : #accessing }
BracketSliderMorph >> sliderColor: newColor [
	"Set the slider colour."
	
	super sliderColor: (self enabled ifTrue: [Color black] ifFalse: [self sliderShadowColor]).
	slider ifNotNil: [slider borderStyle baseColor: Color white]
]

{ #category : #accessing }
BracketSliderMorph >> sliderShadowColor [
	"Answer the color for the slider shadow."
	
	^Color black alpha: 0.6
]

{ #category : #geometry }
BracketSliderMorph >> sliderThickness [
	"Answer the thickness of the slider."
	
	^((self bounds isWide
		ifTrue: [self height]
		ifFalse: [self width]) // 2 max: 8) // 2 * 2 + 1
]

{ #category : #protocol }
BracketSliderMorph >> updateFillStyle [
	"Update the fill style directions."

	|b fs|
	fs := self fillStyle.
	fs isOrientedFill ifTrue: [
		b := self innerBounds.
		fs origin: b topLeft.
		fs direction: (b isWide
			ifTrue: [b width@0]
			ifFalse: [0@b height])]
]
