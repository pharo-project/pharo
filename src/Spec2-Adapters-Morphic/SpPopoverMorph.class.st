Class {
	#name : #SpPopoverMorph,
	#superclass : #PanelMorph,
	#instVars : [
		'position',
		'relativeTo'
	],
	#category : #'Spec2-Adapters-Morphic-Support'
}

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
	y := aRect origin y.

	^ x @ y	
]

{ #category : #private }
SpPopoverMorph >> calculatePositionRightFrom: aRect [
	| x y |
	
	x := aRect corner x + self separatatedBy.
	y := aRect origin y.

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
	self extent: aMorph extent + (self borderWidth * 2) + (aMorph borderWidth * 2).
	self addMorphBack: aMorph.
	 "I need this to calculate properly the size (otherwise I cannot position 
	  the popover correctly"
	self computeFullBounds
]

{ #category : #initialization }
SpPopoverMorph >> defaultColor [

	^ self theme lightBaseColor
]

{ #category : #initialization }
SpPopoverMorph >> initialize [

	super initialize.
	self 
		changeTableLayout;
		listDirection: #leftToRight;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		"onAnnouncement: MorphLostFocus do: [ :ann | ann morph popdown ];"
		beSticky.
	self bePositionBottom
]

{ #category : #api }
SpPopoverMorph >> popdown [
	
	self delete
]

{ #category : #api }
SpPopoverMorph >> popup [

	self popupPointingTo: self relativeTo bounds
]

{ #category : #api }
SpPopoverMorph >> popupPointingTo: aRectangle [

	self setPositionFrom: aRectangle.
	self openInWorld.
	self takeKeyboardFocus
]

{ #category : #accessing }
SpPopoverMorph >> relativeTo [ 

	^ relativeTo
]

{ #category : #private }
SpPopoverMorph >> separatatedBy [

	^ 5
]

{ #category : #private }
SpPopoverMorph >> setPositionFrom: aRect [

	self position: (self calculatePositionFrom: aRect)

	
]
