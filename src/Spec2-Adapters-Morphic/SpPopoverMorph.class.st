"
I am a popover morph, which is a morph who will pop in relation to another morph (or/and in a relative position). 
It has a position: top, left, bottom and right (default is right).
It will be dismissed if click outside or keyboard focus translate to another place other than its children.
"
Class {
	#name : #SpPopoverMorph,
	#superclass : #Morph,
	#instVars : [
		'position',
		'relativeTo',
		'showArrow',
		'arrowSize',
		'deleteMorph'
	],
	#category : #'Spec2-Adapters-Morphic-Support'
}

{ #category : #private }
SpPopoverMorph >> arrowBottomVertices [
	| rect |
	
	rect := ((self left + (self width / 2)) - self borderWidth)@(self top) extent: self arrowSize@(self borderWidth / 2).
	^ { 
		rect bottomLeft. 
		rect topCenter.
		rect bottomRight.
	 }
]

{ #category : #private }
SpPopoverMorph >> arrowLeftVertices [
	| rect |
	
	"rect := (self right - (self borderWidth / 2))@((self top + (self height / 2)) - (self arrowSize / 2)) extent: (self borderWidth / 2)@self arrowSize."
	rect := (self right - (self borderWidth / 2))@(self top + (self borderWidth * 1.5)) extent: (self borderWidth / 2)@self arrowSize.
	
	^ { 
		rect topLeft. 
		rect rightCenter.
		rect bottomLeft.
	 }
]

{ #category : #private }
SpPopoverMorph >> arrowRightVertices [
	| rect |
	
	"rect := self left@((self top + (self height / 2)) - (self arrowSize / 2)) extent: (self borderWidth / 2)@self arrowSize."
	rect := self left@(self top + (self borderWidth * 1.5)) extent: (self borderWidth / 2)@self arrowSize.

	
	^ { 
		rect topRight. 
		rect leftCenter.
		rect bottomRight.
	 }
]

{ #category : #accessing }
SpPopoverMorph >> arrowSize [

	^ arrowSize ifNil: [ 20 ]
]

{ #category : #accessing }
SpPopoverMorph >> arrowSize: aNumber [

	arrowSize := aNumber
]

{ #category : #private }
SpPopoverMorph >> arrowTopVertices [
	| rect |
	
	rect := ((self left + (self width / 2)) - self borderWidth)@(self bottom - (self borderWidth / 2)) extent: self arrowSize@(self borderWidth / 2).
	^ { 
		rect topLeft. 
		rect bottomCenter.
		rect topRight.
	 }
]

{ #category : #private }
SpPopoverMorph >> arrowVertices [

	position = #bottom ifTrue: [ ^ self arrowBottomVertices ].
	position = #right ifTrue: [ ^ self arrowRightVertices ].
	position = #top ifTrue: [ ^ self arrowTopVertices ].
	position = #left ifTrue: [ ^ self arrowLeftVertices ].
	
	self error: 'Should not arrive here'
]

{ #category : #accessing }
SpPopoverMorph >> bePositionBottom [

	position := #bottom
]

{ #category : #accessing }
SpPopoverMorph >> bePositionLeft [

	position := #left
]

{ #category : #accessing }
SpPopoverMorph >> bePositionRight [

	position := #right
]

{ #category : #accessing }
SpPopoverMorph >> bePositionTop [

	position := #top
]

{ #category : #accessing }
SpPopoverMorph >> beRelativeTo: aMorph [

	relativeTo := aMorph
]

{ #category : #private }
SpPopoverMorph >> calculatePositionBottomFrom: aRect [
	| x y |
	
	y := aRect corner y + self separatatedBy.
	x := aRect corner x - ((aRect width + self width) // 2).

	^ x @ y
	
]

{ #category : #private }
SpPopoverMorph >> calculatePositionFrom: aRect [

	position = #top ifTrue: [ ^ self calculatePositionTopFrom: aRect ].
	position = #left ifTrue: [ ^ self calculatePositionLeftFrom: aRect ].
	position = #bottom ifTrue: [ ^ self calculatePositionBottomFrom: aRect ].
	position = #right ifTrue: [ ^ self calculatePositionRightFrom: aRect ].
	
	self error: 'Should not arrive here'


	
]

{ #category : #private }
SpPopoverMorph >> calculatePositionLeftFrom: aRect [
	| x y |
	
	x := aRect origin x - self separatatedBy - self width.
	y := aRect origin y - self borderWidth.

	^ x @ y	
]

{ #category : #private }
SpPopoverMorph >> calculatePositionRightFrom: aRect [
	| x y |
	
	x := aRect corner x + self separatatedBy.
	y := aRect origin y - self borderWidth.

	^ x @ y
]

{ #category : #private }
SpPopoverMorph >> calculatePositionTopFrom: aRect [
	| x y |
	
	y := aRect origin y - self separatatedBy - self height.
	x := aRect corner x - ((aRect width + self width) // 2).

	^ x @ y	
]

{ #category : #accessing }
SpPopoverMorph >> contentMorph: aMorph [ 

	self removeAllMorphs.
	self addMorphBack: aMorph
]

{ #category : #accessing }
SpPopoverMorph >> defaultColor [

	^ self theme lightBaseColor
]

{ #category : #'submorphs-add/remove' }
SpPopoverMorph >> delete [

	self activeHand ifNotNil: [ :hand | 
		deleteMorph ifNotNil: [ 
			hand removeMouseListener: deleteMorph ] ].
	super delete
]

{ #category : #drawing }
SpPopoverMorph >> drawArrowOn: canvas [
		
	canvas 
		drawPolygon: self arrowVertices
		fillStyle: self color
]

{ #category : #drawing }
SpPopoverMorph >> drawBorderOn: canvas [
	
	canvas 
		fillRectangle: (self bounds insetBy: self borderWidth / 2) 
		color: self color
]

{ #category : #drawing }
SpPopoverMorph >> drawOn: canvas [

	self drawBorderOn: canvas.
	super drawOn: canvas.
	showArrow ifTrue: [ 
		self drawArrowOn: canvas ]
]

{ #category : #accessing }
SpPopoverMorph >> hideArrow [

	showArrow := false
]

{ #category : #initialization }
SpPopoverMorph >> initialize [

	super initialize.
	self 
		changeTableLayout;
		listDirection: #leftToRight;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		borderWidth: 10;
		borderColor: Color transparent;
		listCentering: #center;
		beSticky.
	self bePositionBottom.
	self showArrow
]

{ #category : #api }
SpPopoverMorph >> popdown [
	
	self delete
]

{ #category : #api }
SpPopoverMorph >> popup [

	self popupPointingTo: self relativeTo boundsInWorld
]

{ #category : #api }
SpPopoverMorph >> popupPointingTo: aRectangle [

	self setSize.
	self setPositionFrom: aRectangle.
	self openInWorld.
	self takeKeyboardFocus.
	self registerToDeleteEvents
]

{ #category : #private }
SpPopoverMorph >> registerToDeleteEvents [

	deleteMorph ifNotNil: [ self activeHand removeMouseListener: deleteMorph ].
	self activeHand addMouseListener: (deleteMorph := SpPopoverDeleteListenerMorph new 
		on: #mouseDown send: #validateMousePosition: to: self;
		on: #mouseUp send: #validateMousePosition: to: self;
		yourself)
]

{ #category : #accessing }
SpPopoverMorph >> relativeTo [ 

	^ relativeTo
]

{ #category : #private }
SpPopoverMorph >> separatatedBy [

	^ 0
]

{ #category : #private }
SpPopoverMorph >> setPositionFrom: aRect [

	self position: (self calculatePositionFrom: aRect)

	
]

{ #category : #private }
SpPopoverMorph >> setSize [
	| morph |
	
	morph := self submorphs first.
	self extent: morph extent 
		+ (self borderWidth * 2) 
		+ (morph borderWidth * 2) 
		+ (self cellInset * 2)
		+ (4@0) "a magic to correctly display (otherwise it will add an horizontal 
		scrollbar to contents)".
	 "I need this to calculate properly the size (otherwise I cannot position 
	  the popover correctly"
	self computeFullBounds
]

{ #category : #accessing }
SpPopoverMorph >> showArrow [

	showArrow := true
]

{ #category : #private }
SpPopoverMorph >> validateMousePosition: evt [
	"Handle a mouse up event."
	
	(self fullContainsPoint: evt position) ifTrue: [ ^ self ].
	"Mouse up outside. Release eventual focus and delete if pop up."
	"evt hand releaseMouseFocus: self."
	self currentWorld defer: [ self delete ]
]
