"
I am the superclass of a hierarchy of morph specialized in allowing the user to resize or rearrange windows and panes.
"
Class {
	#name : #AbstractResizerMorph,
	#superclass : #Morph,
	#instVars : [
		'dotColor',
		'handleColor',
		'lastMouse'
	],
	#category : #Morphic-Widgets-Windows-Resizing
}

{ #category : #actions }
AbstractResizerMorph >> adoptPaneColor: paneColor [
	"Just get the resizer fill style for the theme."
	
	paneColor ifNil: [^super adoptPaneColor: paneColor].
	self fillStyle: (self theme resizerGripNormalFillStyleFor: self)
]

{ #category : #actions }
AbstractResizerMorph >> dotColor [

	^ dotColor ifNil: [self setDefaultColors. dotColor]
]

{ #category : #actions }
AbstractResizerMorph >> handleColor [

	^ handleColor ifNil: [self setDefaultColors. handleColor]
]

{ #category : #'event handling' }
AbstractResizerMorph >> handlesMouseDown: anEvent [

	^ true
]

{ #category : #'event handling' }
AbstractResizerMorph >> handlesMouseOver: anEvent [

	^ true
	
]

{ #category : #initialization }
AbstractResizerMorph >> initialize [

	super initialize.
	self color: Color transparent
]

{ #category : #testing }
AbstractResizerMorph >> isCursorOverHandle [

	^ true
]

{ #category : #'event handling' }
AbstractResizerMorph >> mouseDown: anEvent [

	lastMouse := anEvent cursorPoint
]

{ #category : #'event handling' }
AbstractResizerMorph >> mouseEnter: anEvent [

	self isCursorOverHandle ifTrue:
		[self setInverseColors.
		self shouldInvalidateOnMouseTransition ifTrue: [self changed]. "avoid unnecessary invalidation"
		anEvent hand showTemporaryCursor: self resizeCursor]
]

{ #category : #'event handling' }
AbstractResizerMorph >> mouseLeave: anEvent [

	anEvent hand showTemporaryCursor: nil.
	self setDefaultColors.
	self shouldInvalidateOnMouseTransition ifTrue: [self changed]. 
	"avoid unnecessary invalidation"
]

{ #category : #'event handling' }
AbstractResizerMorph >> mouseUp: anEvent [
	"Change the cursor back to normal if necessary."
	
	(self bounds containsPoint: anEvent cursorPoint) ifFalse: [
		anEvent hand showTemporaryCursor: nil.
		self
			setDefaultColors;
			changed]
]

{ #category : #actions }
AbstractResizerMorph >> resizeCursor [

	self subclassResponsibility
]

{ #category : #actions }
AbstractResizerMorph >> setDefaultColors [

	handleColor := Color lightGray lighter lighter.
	dotColor := Color gray lighter
]

{ #category : #actions }
AbstractResizerMorph >> setInverseColors [

	handleColor := Color lightGray.
	dotColor := Color white
]

{ #category : #testing }
AbstractResizerMorph >> shouldDraw [
	"Answer whether the resizer should be drawn."
	
	^self fillStyle isTransparent not
]

{ #category : #testing }
AbstractResizerMorph >> shouldInvalidateOnMouseTransition [
	"Answer whether the resizer should be invalidated
	when the mouse enters or leaves."
	
	^false
]
