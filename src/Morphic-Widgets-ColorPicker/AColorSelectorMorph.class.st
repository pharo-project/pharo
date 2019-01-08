"
ColorComponentSelector showing an alpha gradient over a hatched background.
"
Class {
	#name : #AColorSelectorMorph,
	#superclass : #BracketSliderMorph,
	#category : #'Morphic-Widgets-ColorPicker'
}

{ #category : #accessing }
AColorSelectorMorph >> color: aColor [
	"Set the gradient colors."
	
	super color: aColor beOpaque. 
	self fillStyle: self defaultFillStyle
]

{ #category : #protocol }
AColorSelectorMorph >> defaultFillStyle [
	"Answer the hue gradient."

	^(GradientFillStyle colors: {self color alpha: 0. self color})
		origin: self topLeft;
		direction: (self bounds isWide
					ifTrue: [self width@0]
					ifFalse: [0@self height])
]

{ #category : #drawing }
AColorSelectorMorph >> drawOn: aCanvas [
	"Draw a hatch pattern first."

	aCanvas
		fillRectangle: self innerBounds
		fillStyle: (InfiniteForm with: self hatchForm).
	super drawOn: aCanvas

]

{ #category : #'visual properties' }
AColorSelectorMorph >> fillStyle: fillStyle [
	"If it is a color then override with gradient."
	
	fillStyle isColor
		ifTrue: [self color: fillStyle]
		ifFalse: [super fillStyle: fillStyle]
]

{ #category : #private }
AColorSelectorMorph >> hatchForm [
	"Answer a form showing a grid hatch pattern."

	^ColorPresenterMorph hatchForm
]

{ #category : #initialization }
AColorSelectorMorph >> initialize [
	"Initialize the receiver."

	super initialize.
	self
		value: 1.0;
		color: Color black
]
