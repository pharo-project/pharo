Class {
	#name : #GLMBrickBounds,
	#superclass : #Object,
	#instVars : [
		'bounds',
		'innerBounds',
		'outerBounds',
		'widthState',
		'heightState',
		'margin',
		'padding'
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
GLMBrickBounds class >> initialize [	
	Top := 1.
	Right := 2.
	Bottom := 3.
	Left := 4
]

{ #category : #'accessing - bounds' }
GLMBrickBounds >> bounds [

	^ self brickBounds
]

{ #category : #'accessing - bounds' }
GLMBrickBounds >> bounds: aRectangle [
	"changes bounds and marks width or height as dirty if it changes"
	self updateState: aRectangle.
	self brickBounds: aRectangle
]

{ #category : #private }
GLMBrickBounds >> brickBounds [
	"brickBounds give direct access to the bounds Rectangle"
	^ bounds ifNil: [ bounds := 0@0 corner: 50@50 ]

	
]

{ #category : #private }
GLMBrickBounds >> brickBounds: aRectangle [
	"brickBounds give direct access to the bounds Rectangle"
	bounds := aRectangle ceiling.
	self updateCache
]

{ #category : #geometry }
GLMBrickBounds >> extent [

	^ self brickBounds extent
]

{ #category : #geometry }
GLMBrickBounds >> height: anInteger [

	self brickBounds: (self brickBounds withHeight: anInteger)
]

{ #category : #'accessing - bounds' }
GLMBrickBounds >> heightState [

	^ heightState ifNil: [ heightState := #dirty ]
]

{ #category : #'accessing - bounds' }
GLMBrickBounds >> heightState: aSymbol [

	heightState := aSymbol
]

{ #category : #'accessing - bounds' }
GLMBrickBounds >> innerBounds [

	^ innerBounds ifNil: [ self updateCache. innerBounds ]
]

{ #category : #testing }
GLMBrickBounds >> isHeightClean [

	^ self heightState = #clean
]

{ #category : #testing }
GLMBrickBounds >> isHeightDirty [

	^ self heightState = #dirty
]

{ #category : #testing }
GLMBrickBounds >> isWidthClean [

	^ self widthState = #clean
]

{ #category : #testing }
GLMBrickBounds >> isWidthDirty [

	^ self widthState = #dirty
]

{ #category : #'accessing - margin/padding' }
GLMBrickBounds >> margin [

	"top right bottom left"
	^ margin ifNil: [ margin := Array braceWith: 0 with: 0 with: 0 with: 0 ]

]

{ #category : #'accessing - margin/padding' }
GLMBrickBounds >> margin: margins [
	"The margins property can have from one to four values:
	
	margin: #(25 50 75 100)
		top margin is 25px
		right margin is 50px
		bottom margin is 75px
		left margin is 100px
		
	margin: #(25 50 75)
		top margin is 25px
		right and left margins are 50px
		bottom margin is 75px
		
	margin: #(25 50)
		top and bottom margins are 25px
		right and left margins are 50px
		
	margin: #(25)
		all four margins are 25px
		
	margin: 25
		all four margins are 25px"
	
	margins ifNil: [ ^ self margin: #(0) ].
	
	margins isCollection ifTrue: [
		margins isEmpty ifTrue: [ ^ self margin: #(0) ].
		self
			marginTop: (margins at: 1);
			marginRight: (margins at: 1);
			marginBottom: (margins at: 1);
			marginLeft: (margins at: 1).
		margins size > 1 ifTrue: [
			self
				marginRight: (margins at: 2);
				marginLeft: (margins at: 2) ].
		margins size > 2 ifTrue: [
			self marginBottom: (margins at: 3) ].
		margins size > 3 ifTrue: [
			self marginLeft: (margins at: 4) ] ]
	ifFalse: [ ^ self margin: {margins} ].
	
	self updateCache
]

{ #category : #'accessing - margin/padding' }
GLMBrickBounds >> marginBottom [

	^ self margin at: Bottom
]

{ #category : #'accessing - margin/padding' }
GLMBrickBounds >> marginBottom: anInteger [

	self translateY: self marginBottom - anInteger.
	self margin at: Bottom put: anInteger.
	self updateCache
]

{ #category : #'accessing - margin/padding' }
GLMBrickBounds >> marginLeft [

	^ self margin at: Left
]

{ #category : #'accessing - margin/padding' }
GLMBrickBounds >> marginLeft: anInteger [

	self translateX: anInteger - self marginLeft.
	self margin at: Left put: anInteger.
	self updateCache
]

{ #category : #'accessing - margin/padding' }
GLMBrickBounds >> marginRight [

	^ self margin at: Right
]

{ #category : #'accessing - margin/padding' }
GLMBrickBounds >> marginRight: anInteger [

	self translateX: self marginRight - anInteger.
	self margin at: Right put: anInteger.
	self updateCache
]

{ #category : #'accessing - margin/padding' }
GLMBrickBounds >> marginTop [

	^ self margin at: Top
]

{ #category : #'accessing - margin/padding' }
GLMBrickBounds >> marginTop: anInteger [

	self translateY: anInteger - self marginTop.
	self margin at: Top put: anInteger.
	self updateCache
]

{ #category : #marking }
GLMBrickBounds >> markHeightClean [

	self heightState: #clean
]

{ #category : #marking }
GLMBrickBounds >> markHeightDirty [

	self heightState: #dirty
]

{ #category : #marking }
GLMBrickBounds >> markWidthClean [

	self widthState: #clean
]

{ #category : #marking }
GLMBrickBounds >> markWidthDirty [

	self widthState: #dirty
]

{ #category : #'accessing - bounds' }
GLMBrickBounds >> outerBounds [

	^ outerBounds ifNil: [ self updateCache. outerBounds ]
]

{ #category : #geometry }
GLMBrickBounds >> outerHeight: anInteger [

	self brickBounds: (self brickBounds withHeight: (anInteger - self marginTop - self marginBottom))
]

{ #category : #geometry }
GLMBrickBounds >> outerOrigin: aPoint [
	
	self brickBounds: (aPoint + (self marginLeft @ self marginTop) extent: self extent )
]

{ #category : #geometry }
GLMBrickBounds >> outerWidth: anInteger [

	self brickBounds: (self brickBounds withWidth: (anInteger - self marginLeft - self marginRight))
]

{ #category : #'accessing - margin/padding' }
GLMBrickBounds >> padding [

	"top right bottom left"
	^ padding ifNil: [ padding := Array braceWith: 0 with: 0 with: 0 with: 0 ]

]

{ #category : #'accessing - margin/padding' }
GLMBrickBounds >> padding: paddings [
	"The paddings property can have from one to four values:
	
	padding: #(25 50 75 100)
		top padding is 25px
		right padding is 50px
		bottom padding is 75px
		left padding is 100px
		
	padding: #(25 50 75)
		top padding is 25px
		right and left paddings are 50px
		bottom padding is 75px
		
	padding: #(25 50)
		top and bottom paddings are 25px
		right and left paddings are 50px
		
	padding: #(25)
		all four paddings are 25px
		
	padding: 25
		all four paddings are 25px"
	
	paddings ifNil: [ ^ self padding: #(0) ].
	
	paddings isCollection ifTrue: [
		paddings isEmpty ifTrue: [ ^ self padding: #(0) ].
		self
			paddingTop: (paddings at: Top);
			paddingRight: (paddings at: Top);
			paddingBottom: (paddings at: Top);
			paddingLeft: (paddings at: Top).
		paddings size > Top ifTrue: [
			self
				paddingRight: (paddings at: Right);
				paddingLeft: (paddings at: Right) ].
		paddings size > Right ifTrue: [
			self paddingBottom: (paddings at: Bottom) ].
		paddings size > Bottom ifTrue: [
			self paddingLeft: (paddings at: Left) ] ]
	ifFalse: [ ^ self padding: { paddings } ].
	self updateCache
]

{ #category : #'accessing - margin/padding' }
GLMBrickBounds >> paddingBottom [

	^ self padding at: Bottom
]

{ #category : #'accessing - margin/padding' }
GLMBrickBounds >> paddingBottom: anInteger [

	self padding at: Bottom put: anInteger.
	self updateCache
]

{ #category : #'accessing - margin/padding' }
GLMBrickBounds >> paddingLeft [

	^ self padding at: Left
]

{ #category : #'accessing - margin/padding' }
GLMBrickBounds >> paddingLeft: anInteger [

	self padding at: Left put: anInteger.
	self updateCache
]

{ #category : #'accessing - margin/padding' }
GLMBrickBounds >> paddingRight [

	^ self padding at: Right
]

{ #category : #'accessing - margin/padding' }
GLMBrickBounds >> paddingRight: anInteger [

	self padding at: Right put: anInteger.
	self updateCache
]

{ #category : #'accessing - margin/padding' }
GLMBrickBounds >> paddingTop [

	^ self padding at: Top
]

{ #category : #'accessing - margin/padding' }
GLMBrickBounds >> paddingTop: anInteger [

	self padding at: Top put: anInteger.
	self updateCache
]

{ #category : #transformations }
GLMBrickBounds >> translateBy: aPoint [

	self brickBounds: (self brickBounds translateBy: aPoint)
]

{ #category : #transformations }
GLMBrickBounds >> translateX: anInteger [

	self translateBy: anInteger@0
]

{ #category : #transformations }
GLMBrickBounds >> translateY: anInteger [

	self translateBy: 0@anInteger
]

{ #category : #private }
GLMBrickBounds >> updateCache [
	|left top right bottom |
	
	left := self brickBounds origin x.
	top := self brickBounds origin y.
	right := self brickBounds corner x.
	bottom := self brickBounds corner y.
	
	innerBounds := ((left + self paddingLeft)@(top + self paddingTop)) corner: ((right - self paddingRight)@(bottom - self paddingBottom)).
	outerBounds := ((left - self marginLeft)@(top - self marginTop)) corner: ((right + self marginRight)@(bottom + self marginBottom)).
]

{ #category : #private }
GLMBrickBounds >> updateState: aRectangle [

	self brickBounds width = aRectangle width ifFalse: [ self markWidthDirty ].
	self brickBounds height = aRectangle height ifFalse: [ self markHeightDirty ]
]

{ #category : #geometry }
GLMBrickBounds >> width: anInteger [

	self brickBounds: (self brickBounds withWidth: anInteger)
]

{ #category : #'accessing - margin/padding' }
GLMBrickBounds >> widthState [

	^ widthState ifNil: [ widthState := #dirty ]
]

{ #category : #'accessing - margin/padding' }
GLMBrickBounds >> widthState: aSymbol [

	widthState := aSymbol
]
