"
A resizer morph for a fast table.
I can be added to any place but in current implementation I'm used in table headers (because would be expensive to add to each cell).
"
Class {
	#name : #FTColumnResizerMorph,
	#superclass : #AbstractResizerMorph,
	#instVars : [
		'traceMorph',
		'oldColor',
		'container',
		'leftColumn',
		'rightColumn'
	],
	#category : #Morphic-Widgets-FastTable
}

{ #category : #'instance creation' }
FTColumnResizerMorph class >> container: aTableContainerMorph left: aColumn right: otherColumn [
	^ self basicNew 
		initializeContainer: aTableContainerMorph 
			left: aColumn 
			right: otherColumn;
		yourself
]

{ #category : #'instance creation' }
FTColumnResizerMorph class >> new [
	^ self error: 'Use #container:left:right:'
]

{ #category : #accessing }
FTColumnResizerMorph class >> resizerWidth [
	^ 8
]

{ #category : #'private - testing' }
FTColumnResizerMorph >> canResizeColumn [
	^ container owner isResizable

]

{ #category : #submorphs-add/remove }
FTColumnResizerMorph >> delete [
	super delete.
	self release.
	
]

{ #category : #drawing }
FTColumnResizerMorph >> drawOn: aCanvas [
	(owner notNil and: [owner containsPoint: self position])
		ifTrue: [super drawOn: aCanvas]

]

{ #category : #initialization }
FTColumnResizerMorph >> initializeContainer: aTableContainerMorph left: aColumn right: otherColumn [
	self initialize.
	self width: self class resizerWidth.
	container := aTableContainerMorph.
	leftColumn := aColumn.
	rightColumn := otherColumn
	
]

{ #category : #'event handling' }
FTColumnResizerMorph >> mouseDown: anEvent [ 
	"A mouse button has been pressed.
	Update the color for feedback and store the mouse
	position and relative offset to the receiver."
	| cp |
	self canResizeColumn ifFalse: [^ self].
	(self bounds containsPoint: anEvent cursorPoint)
		ifTrue: [oldColor := self color.
			self setGrabbedColor].
	cp := anEvent cursorPoint.
	lastMouse := {cp. cp - self position}
]

{ #category : #'event handling' }
FTColumnResizerMorph >> mouseEnter: anEvent [
	self canResizeColumn ifFalse: [^ self].
	(owner notNil and: [owner bounds containsPoint: anEvent position]) 
		ifTrue: [super mouseEnter: anEvent]
]

{ #category : #'event handling' }
FTColumnResizerMorph >> mouseLeave: anEvent [
	self canResizeColumn ifFalse: [^ self].
	super mouseLeave: anEvent
]

{ #category : #'event handling' }
FTColumnResizerMorph >> mouseMove: anEvent [ 
	self canResizeColumn ifFalse: [^ self].
	anEvent hand temporaryCursor ifNil: [^ self].
	traceMorph ifNil: [
		traceMorph := Morph newBounds: (self bounds withHeight: container height).
		traceMorph color: Smalltalk ui theme fastTableColumnResizingColor.
		traceMorph borderWidth: 0.
		container addMorph: traceMorph].
	traceMorph position: (anEvent cursorPoint x - lastMouse second x) @ traceMorph position y
]

{ #category : #Polymorph-Widgets }
FTColumnResizerMorph >> mouseUp: anEvent [
	"Change the cursor back to normal if necessary and change the color back to normal."
	
	self canResizeColumn ifFalse: [^ self].
	(self bounds containsPoint: anEvent cursorPoint)
		ifFalse: [anEvent hand showTemporaryCursor: nil].
	self updateFromEvent: anEvent.
	traceMorph ifNotNil: [traceMorph delete. traceMorph := nil].
	self adoptPaneColor: self paneColor.
	self triggerEvent: #mouseUp
]

{ #category : #style }
FTColumnResizerMorph >> normalFillStyle [
	"Return the normal fillstyle for the receiver."
	^ self theme morphTreeSplitterNormalFillStyleFor: self
]

{ #category : #style }
FTColumnResizerMorph >> pressedFillStyle [
	"Return the pressed fillStyle of the receiver."
	^ self theme morphTreeSplitterPressedFillStyleFor: self
]

{ #category : #submorphs-add/remove }
FTColumnResizerMorph >> release [
	traceMorph := nil.
	oldColor := nil.
	container := nil.
	self releaseActionMap. "we are not sure if we need it"
	super release.
	
]

{ #category : #actions }
FTColumnResizerMorph >> resizeCursor [
	^ Cursor resizeForEdge: #left
]

{ #category : #private }
FTColumnResizerMorph >> setGrabbedColor [
	"Set the color of the receiver when it is grabbed."
	self fillStyle: Smalltalk ui theme fastTableColumnSplitterSelectedColor
]

{ #category : #Polymorph-Widgets }
FTColumnResizerMorph >> shouldDraw [
	^ true
]

{ #category : #accessing }
FTColumnResizerMorph >> splitsTopAndBottom [
	^ false
]

{ #category : #Polymorph-Widgets }
FTColumnResizerMorph >> themeChanged [
	"Update the fill style."
	self fillStyle: self normalFillStyle.
	super themeChanged
]

{ #category : #'event handling' }
FTColumnResizerMorph >> updateFromEvent: anEvent [ 
	"Update the splitter and attached morph positions from
	the mouse event.
	Take into account the mouse down offset."
	| delta |
	
	delta := (anEvent cursorPoint - lastMouse first) x.
	"if positive, delta cannot be bigger than right width (plus a variation) 
	 to prevent column overlap."
	(delta > 0 and: [ delta > (rightColumn width - (self width) - 5) ])
		ifTrue: [ delta := rightColumn width - (self width) - 5 ].
	"if negative, -delta cannot be bigger than left width (plus a variation) 
	 to prevent column overlap."
	(delta < 0 and: [ delta negated > (leftColumn width - (self width) - 5) ])
		ifTrue: [ delta := (leftColumn width - (self width) - 5) negated ].

	leftColumn column width: (leftColumn column width ifNil: [ leftColumn width ]) + delta.
	rightColumn column width: (rightColumn column width ifNil: [ rightColumn width ]) - delta.	
	container changed
]
