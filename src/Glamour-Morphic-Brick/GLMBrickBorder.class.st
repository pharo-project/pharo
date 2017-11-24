Class {
	#name : #GLMBrickBorder,
	#superclass : #Object,
	#instVars : [
		'width',
		'color'
	],
	#classVars : [
		'Bottom',
		'Left',
		'Right',
		'Top'
	],
	#category : #'Glamour-Morphic-Brick-Utils'
}

{ #category : #'class initialization' }
GLMBrickBorder class >> initialize [
	Top := 1.
	Right := 2.
	Bottom := 3.
	Left := 4
]

{ #category : #color }
GLMBrickBorder >> color [

	^ color ifNil: [ color := Array braceWith: Color transparent with: Color transparent with: Color transparent with: Color transparent ]
]

{ #category : #color }
GLMBrickBorder >> color: colors [
	"The colors property may have from one to four values:
	
	color: { Color red . Color blue . Color green . Color yellow }
		top color is red
		right color is blue
		bottom color is green
		left color is yellow
		
	color: { Color red . Color blue . Color green }
		top color is red
		right and left colors are blue
		bottom color is green
		
	color: { Color red . Color blue }
		top and bottom colors are red
		right and left colors are blue
		
	color: { Color red }
		all four colors are red
		
	color: Color red
		all four colors are red"
	
	colors ifNil: [ ^ self color: #(0) ].
	
	colors isCollection ifTrue: [
		colors isEmpty ifTrue: [ ^ self color: #(0) ].
		self
			colorTop: (colors at: 1);
			colorRight: (colors at: 1);
			colorBottom: (colors at: 1);
			colorLeft: (colors at: 1).
		colors size > 1 ifTrue: [
			self
				colorRight: (colors at: 2);
				colorLeft: (colors at: 2) ].
		colors size > 2 ifTrue: [
			self colorBottom: (colors at: 3) ].
		colors size > 3 ifTrue: [
			self colorLeft: (colors at: 4) ] ]
	ifFalse: [ ^ self color: { colors } ].
]

{ #category : #color }
GLMBrickBorder >> colorBottom [

	^ self color at: Bottom
]

{ #category : #color }
GLMBrickBorder >> colorBottom: aColor [

	self color at: Bottom put: aColor
]

{ #category : #color }
GLMBrickBorder >> colorLeft [

	^ self color at: Left
]

{ #category : #color }
GLMBrickBorder >> colorLeft: aColor [
	"aColor may be any object, it will be called with brickValue: aBrick"
	self color at: Left put: aColor
]

{ #category : #color }
GLMBrickBorder >> colorRight [

	^ self color at: Right
]

{ #category : #color }
GLMBrickBorder >> colorRight: aColor [

	self color at: Right put: aColor
]

{ #category : #color }
GLMBrickBorder >> colorTop [

	^ self color at: Top
]

{ #category : #color }
GLMBrickBorder >> colorTop: aColor [

	self color at: Top put: aColor
]

{ #category : #rendering }
GLMBrickBorder >> renderBorderOf: aBrick on: aCanvas [
	|canvas widthLeft widthRight widthTop widthBottom colorLeft colorRight colorTop colorBottom|
	
	canvas := aCanvas.
	
	widthLeft := self widthLeft brickValue: aBrick.
	widthRight := self widthRight brickValue: aBrick.
	widthTop := self widthTop brickValue: aBrick.
	widthBottom := self widthBottom brickValue: aBrick.
	
	colorLeft := self colorLeft brickValue: aBrick.
	colorRight := self colorRight brickValue: aBrick.
	colorTop := self colorTop brickValue: aBrick.
	colorBottom := self colorBottom brickValue: aBrick.
	
	canvas
		fillRectangle: (aBrick brickBounds origin extent: ((aBrick brickBounds width - widthRight) @ widthTop))
		color: colorTop.
		
	canvas
		fillRectangle: ((aBrick brickBounds right - widthRight) @ aBrick brickBounds top corner: (aBrick brickBounds corner - (0@widthBottom)))
		color: colorRight.
		
	canvas
		fillRectangle: ((aBrick brickBounds bottomLeft - (widthLeft negated @ widthBottom)) corner: aBrick brickBounds corner)
		color: colorBottom.
		
	canvas
		fillRectangle: ((aBrick brickBounds origin + (0@widthTop)) corner: (aBrick brickBounds bottomLeft + (widthLeft@0)))
		color: colorLeft.
]

{ #category : #width }
GLMBrickBorder >> width [

	^ width ifNil: [ width := Array braceWith: 0 with: 0 with: 0 with: 0 ]
]

{ #category : #width }
GLMBrickBorder >> width: widths [
	"The widths property can have from one to four values:
	
	width: #(25 50 75 100)
		top width is 25px
		right width is 50px
		bottom width is 75px
		left width is 100px
		
	width: #(25 50 75)
		top width is 25px
		right and left widths are 50px
		bottom width is 75px
		
	width: #(25 50)
		top and bottom widths are 25px
		right and left widths are 50px
		
	width: #(25)
		all four widths are 25px
		
	width: 25
		all four widths are 25px"
	
	widths ifNil: [ ^ self width: #(0) ].
	
	widths isCollection ifTrue: [
		widths isEmpty ifTrue: [ ^ self width: #(0) ].
		self
			widthTop: (widths at: 1);
			widthRight: (widths at: 1);
			widthBottom: (widths at: 1);
			widthLeft: (widths at: 1).
		widths size > 1 ifTrue: [
			self
				widthRight: (widths at: 2);
				widthLeft: (widths at: 2) ].
		widths size > 2 ifTrue: [
			self widthBottom: (widths at: 3) ].
		widths size > 3 ifTrue: [
			self widthLeft: (widths at: 4) ] ]
	ifFalse: [ ^ self width: {widths} ]
]

{ #category : #width }
GLMBrickBorder >> widthBottom [

	^ self width at: Bottom
]

{ #category : #width }
GLMBrickBorder >> widthBottom: anInteger [

	self width at: Bottom put: anInteger
]

{ #category : #width }
GLMBrickBorder >> widthLeft [

	^ self width at: Left
]

{ #category : #width }
GLMBrickBorder >> widthLeft: anInteger [

	self width at: Left put: anInteger
]

{ #category : #width }
GLMBrickBorder >> widthRight [

	^ self width at: Right
]

{ #category : #width }
GLMBrickBorder >> widthRight: anInteger [

	self width at: Right put: anInteger
]

{ #category : #width }
GLMBrickBorder >> widthTop [

	^ self width at: Top
]

{ #category : #width }
GLMBrickBorder >> widthTop: anInteger [

	self width at: Top put: anInteger
]
