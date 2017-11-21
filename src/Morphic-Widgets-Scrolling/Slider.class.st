Class {
	#name : #Slider,
	#superclass : #MorphicModel,
	#instVars : [
		'slider',
		'value',
		'setValueSelector',
		'sliderShadow',
		'sliderColor',
		'descending'
	],
	#category : #Morphic-Widgets-Scrolling
}

{ #category : #geometry }
Slider >> computeSlider [
	| r |
	r := self roomToMove.
	self descending
		ifFalse:
			[slider position: (bounds isWide
				ifTrue: [r topLeft + ((r width * value) asInteger @ 0)]
				ifFalse: [r topLeft + (0 @ (r height * value)  asInteger)])]
		ifTrue:
			[slider position: (bounds isWide
				ifTrue:	[r bottomRight - ((r width * value) asInteger @ 0)]
				ifFalse:	[r bottomRight - ((0 @ (r height * value) asInteger))])].
	slider extent: self sliderExtent
]

{ #category : #initialization }
Slider >> defaultBorderColor [
	"answer the default border color/fill style for the receiver"
	^ #inset
]

{ #category : #initialization }
Slider >> defaultBorderWidth [
	"answer the default border width for the receiver"
	^ 1
]

{ #category : #initialization }
Slider >> defaultBounds [
"answer the default bounds for the receiver"
	^ 0 @ 0 corner: 16 @ 100
]

{ #category : #initialization }
Slider >> defaultColor [
	"answer the default color/fill style for the receiver"
	^ Color lightGray
]

{ #category : #access }
Slider >> descending [
	"It may happen that an instance variable is nil, for example, if an instance is in the image when the instance is introduced. This test should probably be removed."
	^ descending == true
]

{ #category : #access }
Slider >> descending: aBoolean [
	descending := aBoolean.
	self value: value
]

{ #category : #geometry }
Slider >> extent: newExtent [
	newExtent = bounds extent ifTrue: [^ self].
	bounds isWide
		ifTrue: [super extent: (newExtent x max: self sliderThickness * 2) @ newExtent y]
		ifFalse: [super extent: newExtent x @ (newExtent y max: self sliderThickness * 2)].
	self removeAllMorphs; initializeSlider
]

{ #category : #initialization }
Slider >> initialize [
	"initialize the state of the receiver"
	
	super initialize.
	value := 0.0.
	descending := false.
	self initializeSlider
]

{ #category : #initialization }
Slider >> initializeSlider [
	slider := BorderedMorph newBounds: self totalSliderArea color: self thumbColor.
	sliderShadow := BorderedMorph newBounds: self totalSliderArea
						color: self pagingArea color.
	slider on: #mouseMove send: #scrollAbsolute: to: self.
	slider on: #mouseDown send: #mouseDownInSlider: to: self.
	slider on: #mouseUp send: #mouseUpInSlider: to: self.
	slider 
		borderWidth: 1;
		borderColor: Color lightGray.

	sliderShadow 
		borderWidth: 1;
		borderColor: #inset.
	"(the shadow must have the pagingArea as its owner to highlight properly)"
	self pagingArea addMorph: sliderShadow.
	sliderShadow hide.
	self addMorph: slider.
	self computeSlider.

]

{ #category : #'other events' }
Slider >> mouseDownInSlider: event [ 

	slider borderStyle style == #raised
		ifTrue: [slider borderColor: #inset].
	
	sliderShadow color: self sliderShadowColor.
	sliderShadow cornerStyle: slider cornerStyle.
	sliderShadow bounds: slider bounds.
	sliderShadow show
]

{ #category : #'other events' }
Slider >> mouseUpInSlider: event [ 

	slider borderStyle style == #inset
		ifTrue: [slider borderColor: #raised].
	
	sliderShadow hide
]

{ #category : #access }
Slider >> pagingArea [
	^self
]

{ #category : #geometry }
Slider >> roomToMove [
	^ self totalSliderArea insetBy: (0@0 extent: self sliderExtent)
]

{ #category : #scrolling }
Slider >> scrollAbsolute: event [
	| r p |
	r := self roomToMove.
	bounds isWide
		ifTrue: [r width = 0 ifTrue: [^ self]]
		ifFalse: [r height = 0 ifTrue: [^ self]].
	p := event targetPoint adhereTo: r.
	self descending
		ifFalse:
			[self setValue: (bounds isWide 
				ifTrue: [(p x - r left) asFloat / r width]
				ifFalse: [(p y - r top) asFloat / r height])]
		ifTrue:
			[self setValue: (bounds isWide
				ifTrue: [(r right - p x) asFloat / r width]
				ifFalse:	[(r bottom - p y) asFloat / r height])]
]

{ #category : #initialization }
Slider >> setSlider: aMorph withShadow: anotherMorph [
	

	slider ifNotNil: [ self removeMorph: slider ].
	sliderShadow ifNotNil: [ self pagingArea removeMorph: sliderShadow ].

	slider := aMorph.
	sliderShadow := anotherMorph.
	slider on: #mouseMove send: #scrollAbsolute: to: self.
	slider on: #mouseDown send: #mouseDownInSlider: to: self.
	slider on: #mouseUp send: #mouseUpInSlider: to: self.

	"(the shadow must have the pagingArea as its owner to highlight properly)"
	self pagingArea addMorph: sliderShadow.
	sliderShadow hide.
	self addMorph: slider.
	self computeSlider.
]

{ #category : #'model access' }
Slider >> setValue: newValue [
	"Called internally for propagation to model"
	self value: newValue.
	setValueSelector ifNotNil: [
		^model perform: setValueSelector with: value
		].
]

{ #category : #access }
Slider >> setValueSelector: aSymbol [
	"Directly set the selector to make more flexible."
	
	setValueSelector := aSymbol
]

{ #category : #access }
Slider >> sliderColor [
	"color scheme for the whole slider widget"
	sliderColor ifNil: [^ (color alphaMixed: 0.7 with: Color white) slightlyLighter].
	^ sliderColor
]

{ #category : #access }
Slider >> sliderColor: newColor [

	sliderColor := newColor.
	slider ifNotNil: [slider color: sliderColor]
]

{ #category : #geometry }
Slider >> sliderExtent [
	^ bounds isWide
		ifTrue: [self sliderThickness @ self innerBounds height]
		ifFalse: [self innerBounds width @ self sliderThickness]
]

{ #category : #access }
Slider >> sliderShadowColor [
	^ self sliderColor alphaMixed: 0.2 with: self pagingArea color
]

{ #category : #geometry }
Slider >> sliderThickness [
	^ 7
]

{ #category : #access }
Slider >> thumbColor [
	"Color of the draggable 'thumb'"
	^ self sliderColor
]

{ #category : #geometry }
Slider >> totalSliderArea [
	^ self innerBounds
]

{ #category : #update }
Slider >> updateSliderBounds [
	slider bounds: self totalSliderArea.
	sliderShadow bounds: self totalSliderArea.
	
	self computeSlider.

]

{ #category : #access }
Slider >> value [
	^ value
]

{ #category : #'model access' }
Slider >> value: newValue [
	"Drive the slider position externally..."
	value := newValue min: 1.0 max: 0.0.
	self computeSlider
]
