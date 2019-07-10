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
	#name : #SpPanedResizerMorph,
	#superclass : #AbstractResizerMorph,
	#category : #'Spec2-Adapters-Morphic-Layout'
}

{ #category : #examples }
SpPanedResizerMorph class >> exampleHorizontal [
	<example>
	
	(PanelMorph new 
		layoutPolicy: SpMorphicPanedLayout new;
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
SpPanedResizerMorph class >> exampleVertical [
	<example>
	
	(PanelMorph new 
		layoutPolicy: SpMorphicPanedLayout new;
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
SpPanedResizerMorph class >> new [ 
	
	self error: 'Use #newHorizonal or #newVertical'
]

{ #category : #'instance creation' }
SpPanedResizerMorph class >> newHorizontal [ 
	
	^ SpPanedResizerHorizontalMorph basicNew initialize
]

{ #category : #'instance creation' }
SpPanedResizerMorph class >> newVertical [
	
	^ SpPanedResizerVerticalMorph basicNew initialize
]

{ #category : #accessing }
SpPanedResizerMorph class >> splitterWidth [

	^ 4
]

{ #category : #accessing }
SpPanedResizerMorph >> adoptPaneColor: paneColor [
	"Change our color too."
	
	super adoptPaneColor: paneColor.
	self fillStyle: self normalFillStyle
]

{ #category : #accessing }
SpPanedResizerMorph >> defaultHeight [
	"Answer the default height for the receiver."
	
	^ self class splitterWidth
]

{ #category : #accessing }
SpPanedResizerMorph >> defaultWidth [
	"Answer the default width for the receiver."
	
	^ self class splitterWidth
]

{ #category : #accessing }
SpPanedResizerMorph >> edgeName [
	"Answer the value of edgeName"

	^ self subclassResponsibility
]

{ #category : #initialization }
SpPanedResizerMorph >> initialize [
	"Initialize the receiver."
	
	super initialize.
	self
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		setLayoutSizing;
		extent: self defaultWidth @ self defaultHeight		
]

{ #category : #testing }
SpPanedResizerMorph >> isHorizontal [
	"Answer true if the receiver has a horizontal layout."
	
	^ self subclassResponsibility
]

{ #category : #'event handling' }
SpPanedResizerMorph >> mouseDown: anEvent [
	"Remember the receiver and target offsets too."
	
	(self bounds containsPoint: anEvent cursorPoint)
		ifTrue: [ self fillStyle: self pressedFillStyle ].
		
	lastMouse := anEvent cursorPoint - self position.
	self eventHandler ifNotNil: [ :handler |
		handler mouseDown: anEvent fromMorph: self ]
]

{ #category : #'event handling' }
SpPanedResizerMorph >> mouseMove: anEvent [
	"Track the mouse for resizing."
	
	self target ifNil: [ ^ self ] .
	
	self setTargetLayoutSizing.
	self resetLayoutPosition.
	self positionPoint: (anEvent cursorPoint - lastMouse).
	self setTargetBounds
]

{ #category : #'event handling' }
SpPanedResizerMorph >> mouseUp: anEvent [
	"Change the cursor back to normal if necessary and change the color back to normal."
	
	(self bounds containsPoint: anEvent cursorPoint)
		ifFalse: [ anEvent hand showTemporaryCursor: nil ].
	self adoptPaneColor: self paneColor.
	self preserveLayoutPosition
]

{ #category : #style }
SpPanedResizerMorph >> normalFillStyle [
	"Return the normal fillStyle of the receiver."
	
	^self theme splitterNormalFillStyleFor: self
]

{ #category : #accessing }
SpPanedResizerMorph >> positionPoint: aPoint [
	"Reposition based on ptName."
	
	^ self subclassResponsibility
]

{ #category : #private }
SpPanedResizerMorph >> preserveLayoutPosition [

	self owner layoutPolicy preservePositionProportionOn: self owner
]

{ #category : #style }
SpPanedResizerMorph >> pressedFillStyle [
	"Return the pressed fillStyle of the receiver."
	
	^ self theme splitterPressedFillStyleFor: self
]

{ #category : #private }
SpPanedResizerMorph >> resetLayoutPosition [
	"since we are going to move the split bar, layout position (if any) needs to be reseted".
	
	self owner layoutPolicy resetPosition
]

{ #category : #actions }
SpPanedResizerMorph >> resizeCursor [

	^ Cursor resizeForEdge: self edgeName
]

{ #category : #private }
SpPanedResizerMorph >> setLayoutSizing [
	"Adjust the sizing for use within table layouts."

	self subclassResponsibility
]

{ #category : #private }
SpPanedResizerMorph >> setTargetBounds [ 

	self subclassResponsibility
]

{ #category : #private }
SpPanedResizerMorph >> setTargetLayoutSizing [

	self subclassResponsibility
]

{ #category : #testing }
SpPanedResizerMorph >> splitsTopAndBottom [
	
	^ self isHorizontal
]

{ #category : #accessing }
SpPanedResizerMorph >> target [ 
	"answers previous morph in owners submorph list"
	
	^ self owner ifNotNil: [ :aMorph | 
		aMorph submorphs before: self ]
]

{ #category : #private }
SpPanedResizerMorph >> targetAfter [ 

	^ self owner submorphs after: self
]

{ #category : #updating }
SpPanedResizerMorph >> themeChanged [
	"Update the fill style."

	self fillStyle: self normalFillStyle.
	super themeChanged
]
