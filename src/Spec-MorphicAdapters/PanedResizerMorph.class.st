"
I'm a base splitter to be used with a SpecPanedLayout. 
Splitters can be placed vertically or horizontally and my children deal with each of the cases.
The way I work is easy: 

- I need to be placed between two other morphs.
- I also need to act inside a morph with PanedLayout. 

self exampleVertical.
self exampleHorizontal.
"
Class {
	#name : #PanedResizerMorph,
	#superclass : #AbstractResizerMorph,
	#category : #'Spec-MorphicAdapters-Layout'
}

{ #category : #examples }
PanedResizerMorph class >> exampleHorizontal [
	<example>
	
	(PanelMorph new 
		layoutPolicy: PanedLayout new;
		listDirection: #leftToRight;
		addMorphBack: (Morph new 
			color: Color red;
			vResizing: #spaceFill;
			hResizing: #spaceFill;
			yourself);
		addMorphBack: self newHorizontal;
		addMorphBack: (Morph new 
			color: Color blue;
			hResizing: #spaceFill;
			vResizing: #spaceFill;
			yourself);
		openInWindow)
		extent: 500@500
]

{ #category : #examples }
PanedResizerMorph class >> exampleVertical [
	<example>
	
	(PanelMorph new 
		layoutPolicy: PanedLayout new;
		addMorphBack: (Morph new 
			color: Color red;
			vResizing: #spaceFill;
			hResizing: #spaceFill;
			yourself);
		addMorphBack: self newVertical;
		addMorphBack: (Morph new 
			color: Color blue;
			vResizing: #spaceFill;
			hResizing: #spaceFill;
			yourself);
		openInWindow)
		extent: 500@500
]

{ #category : #'instance creation' }
PanedResizerMorph class >> new [ 
	
	self error: 'Use #newHorizonal or #newVertical'
]

{ #category : #'instance creation' }
PanedResizerMorph class >> newHorizontal [ 
	
	^ PanedResizerHorizontalMorph basicNew initialize
]

{ #category : #'instance creation' }
PanedResizerMorph class >> newVertical [
	
	^ PanedResizerVerticalMorph basicNew initialize
]

{ #category : #accessing }
PanedResizerMorph class >> splitterWidth [

	^ 4
]

{ #category : #accessing }
PanedResizerMorph >> adoptPaneColor: paneColor [
	"Change our color too."
	
	super adoptPaneColor: paneColor.
	self fillStyle: self normalFillStyle
]

{ #category : #accessing }
PanedResizerMorph >> defaultHeight [
	"Answer the default height for the receiver."
	
	^ self class splitterWidth
]

{ #category : #accessing }
PanedResizerMorph >> defaultWidth [
	"Answer the default width for the receiver."
	
	^ self class splitterWidth
]

{ #category : #accessing }
PanedResizerMorph >> edgeName [
	"Answer the value of edgeName"

	^ self subclassResponsibility
]

{ #category : #initialization }
PanedResizerMorph >> initialize [
	"Initialize the receiver."
	
	super initialize.
	self
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		setLayoutSizing;
		extent: self defaultWidth @ self defaultHeight		
]

{ #category : #testing }
PanedResizerMorph >> isHorizontal [
	"Answer true if the receiver has a horizontal layout."
	
	^ self subclassResponsibility
]

{ #category : #'event handling' }
PanedResizerMorph >> mouseDown: anEvent [
	"Remember the receiver and target offsets too."
	
	(self bounds containsPoint: anEvent cursorPoint)
		ifTrue: [ self fillStyle: self pressedFillStyle ].
		
	lastMouse := anEvent cursorPoint - self position.
	self eventHandler ifNotNil: [ :handler |
		handler mouseDown: anEvent fromMorph: self ]
]

{ #category : #'event handling' }
PanedResizerMorph >> mouseMove: anEvent [
	"Track the mouse for resizing."
	
	self target ifNil: [ ^ self ] .
	
	self setTargetLayoutSizing.
	self resetLayoutPosition.
	self positionPoint: (anEvent cursorPoint - lastMouse).
	self setTargetBounds
]

{ #category : #'event handling' }
PanedResizerMorph >> mouseUp: anEvent [
	"Change the cursor back to normal if necessary and change the color back to normal."
	
	(self bounds containsPoint: anEvent cursorPoint)
		ifFalse: [ anEvent hand showTemporaryCursor: nil ].
	self adoptPaneColor: self paneColor.
	self preserveLayoutPosition
]

{ #category : #style }
PanedResizerMorph >> normalFillStyle [
	"Return the normal fillStyle of the receiver."
	
	^self theme splitterNormalFillStyleFor: self
]

{ #category : #accessing }
PanedResizerMorph >> positionPoint: aPoint [
	"Reposition based on ptName."
	
	^ self subclassResponsibility
]

{ #category : #private }
PanedResizerMorph >> preserveLayoutPosition [

	self owner layoutPolicy preservePositionProportionOn: self owner
]

{ #category : #style }
PanedResizerMorph >> pressedFillStyle [
	"Return the pressed fillStyle of the receiver."
	
	^ self theme splitterPressedFillStyleFor: self
]

{ #category : #private }
PanedResizerMorph >> resetLayoutPosition [
	"since we are going to move the split bar, layout position (if any) needs to be reseted".
	
	self owner layoutPolicy resetPosition
]

{ #category : #actions }
PanedResizerMorph >> resizeCursor [

	^ Cursor resizeForEdge: self edgeName
]

{ #category : #private }
PanedResizerMorph >> setLayoutSizing [
	"Adjust the sizing for use within table layouts."

	self subclassResponsibility
]

{ #category : #private }
PanedResizerMorph >> setTargetBounds [ 

	self subclassResponsibility
]

{ #category : #private }
PanedResizerMorph >> setTargetLayoutSizing [

	self subclassResponsibility
]

{ #category : #testing }
PanedResizerMorph >> splitsTopAndBottom [
	
	^ self isHorizontal
]

{ #category : #accessing }
PanedResizerMorph >> target [ 
	"answers previous morph in owners submorph list"
	
	^ self owner ifNotNil: [ :aMorph | 
		aMorph submorphs before: self ]
]

{ #category : #private }
PanedResizerMorph >> targetAfter [ 

	^ self owner submorphs after: self
]

{ #category : #updating }
PanedResizerMorph >> themeChanged [
	"Update the fill style."

	self fillStyle: self normalFillStyle.
	super themeChanged
]
