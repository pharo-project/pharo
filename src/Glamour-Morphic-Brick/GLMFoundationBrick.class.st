"
A GLMFoundationBrick is connection layer between brick widgets and morphic.

See GLMBrick class comment for more info.

Instance Variables
	brickRoot:		<Object>

brickRoot
	- xxxxx

"
Class {
	#name : #GLMFoundationBrick,
	#superclass : #Morph,
	#instVars : [
		'brickRoot',
		'vResizing',
		'hResizing'
	],
	#category : #'Glamour-Morphic-Brick-Morphic'
}

{ #category : #'instance creation' }
GLMFoundationBrick class >> root: aBrick [

	^ self new brickRoot: aBrick
]

{ #category : #adding }
GLMFoundationBrick >> addAllBricks: aCollection [

	self brickRoot addAllBricks: aCollection
]

{ #category : #adding }
GLMFoundationBrick >> addBrickBack: aBrick [

	self brickRoot addBrickBack: aBrick.
]

{ #category : #accessing }
GLMFoundationBrick >> brickRoot [

	^ brickRoot
]

{ #category : #accessing }
GLMFoundationBrick >> brickRoot: aBrick [

	self brickRoot ifNotNil: [
		self removeMorph: self brickRoot.
		brickRoot := nil ].
	
	brickRoot := aBrick.
	self addMorphBack: brickRoot.
	brickRoot onThemerChanged
]

{ #category : #private }
GLMFoundationBrick >> brickThemerChanged [

	self brickRoot themerChanged
]

{ #category : #recategorized }
GLMFoundationBrick >> changed [
	"Report that the area occupied by this morph should be redrawn.
	Fixed to include submorphs outside the outerBounds"
	
	self brickRoot changed
]

{ #category : #layouting }
GLMFoundationBrick >> doLayout [

	self brickRoot doLayoutForce.
]

{ #category : #'reflective operations' }
GLMFoundationBrick >> doesNotUnderstand: aMessage [

	^ self brickRoot perform: aMessage selector withArguments: aMessage arguments
]

{ #category : #geometry }
GLMFoundationBrick >> extent: aPoint [
	|left top right bottom|
	
	super extent: aPoint.
	
	left := self brickRoot marginLeft.
	top := self brickRoot marginTop.
	
	right := self extent x - self brickRoot marginRight.
	bottom := self extent y - self brickRoot marginBottom.
	
	self isHFilling ifTrue: [ self brickRoot width: right - left ].
	self isVFilling ifTrue: [ self brickRoot height: bottom - top ].
	self brickRoot wrappedBounds bounds: (left@top corner: (right@bottom)).
	
	self brickRoot doLayoutForce
]

{ #category : #accessing }
GLMFoundationBrick >> fullBounds [
	super fullBounds.
	^ self brickRoot globalClippingBounds
]

{ #category : #drawing }
GLMFoundationBrick >> fullDrawOn: aCanvas [
	"preventing from drawing outside of me"
	super fullDrawOn: (aCanvas copyClipRect: self fullBounds).
]

{ #category : #geometry }
GLMFoundationBrick >> globalBounds [

	^ self bounds
]

{ #category : #geometry }
GLMFoundationBrick >> globalBoundsTranslateFactor [

	^ self globalBounds origin
]

{ #category : #'brick-layout-properties' }
GLMFoundationBrick >> hShrinkWrap [

	hResizing:= #shrinkWrap
]

{ #category : #'brick-layout-properties' }
GLMFoundationBrick >> hSpaceFill [

	hResizing:= #spaceFill.
	self hResizing: #spaceFill
]

{ #category : #initialization }
GLMFoundationBrick >> initialize [
	super initialize.

	vResizing := #spaceFill.
	hResizing := #spaceFill.

	self
		beTransparent;
		brickRoot: GLMBrick new
]

{ #category : #testing }
GLMFoundationBrick >> isFocused [

	^ false
]

{ #category : #testing }
GLMFoundationBrick >> isFoundationBrick [

	^ true
]

{ #category : #testing }
GLMFoundationBrick >> isHFilling [

	^ hResizing = #spaceFill
]

{ #category : #testing }
GLMFoundationBrick >> isHShrinking [

	^ hResizing = #shrinkWrap
]

{ #category : #testing }
GLMFoundationBrick >> isVFilling [

	^ vResizing = #spaceFill
]

{ #category : #testing }
GLMFoundationBrick >> isVShrinking [

	^ vResizing = #shrinkWrap
]

{ #category : #initialize }
GLMFoundationBrick >> outOfWorld: aWorld [
	super outOfWorld: aWorld.
	
	self brickRoot onRemovedFrom: self
]

{ #category : #private }
GLMFoundationBrick >> privateExtent: aPoint [

	super extent: aPoint.
]

{ #category : #private }
GLMFoundationBrick >> privateThemer [

	^ self valueOfProperty: #brickThemer ifAbsentPut: [ nil ]
]

{ #category : #adding }
GLMFoundationBrick >> removeAllBricks [

	self brickRoot removeAllBricks
]

{ #category : #adding }
GLMFoundationBrick >> removeBrick: aBrick [

	self brickRoot removeBrick: aBrick
]

{ #category : #accessing }
GLMFoundationBrick >> themer [

	^ self privateThemer
		ifNotNil: [ self privateThemer ]
		ifNil: [ self owner isNotNil
			ifTrue: [ self owner isBrick ifTrue: [ self owner themer ] ifFalse: [ super brickThemer ] ]
			ifFalse: [ self theme brickThemer ] ]
]

{ #category : #accessing }
GLMFoundationBrick >> themer: aThemer [

	self setProperty: #brickThemer toValue: aThemer.
	self brickRoot themerChanged
]

{ #category : #'brick-layout-properties' }
GLMFoundationBrick >> vShrinkWrap [

	vResizing:= #shrinkWrap
]

{ #category : #'brick-layout-properties' }
GLMFoundationBrick >> vSpaceFill [

	vResizing:= #spaceFill
]
