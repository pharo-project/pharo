"
I am a popper that displays an error message.

GLMErrorPopper simpleErrorPopper
"
Class {
	#name : #GLMErrorPopper,
	#superclass : #GLMPopper,
	#category : #Glamour-Morphic-Widgets-Popper
}

{ #category : #examples }
GLMErrorPopper class >> simpleErrorPopper [
	"
	self simpleErrorPopper
	"

	(GLMErrorPopper new 
		withString: 'error message goes here' 
		from: RubScrolledTextMorph new) openInWorld
]

{ #category : #initialization }
GLMErrorPopper >> initialize [
	super initialize.
	self initializeTextMorph
]

{ #category : #initialization }
GLMErrorPopper >> initializeTextMorph [
	textMorph := RubScrolledTextMorph new.
	textMorph
		beReadOnly;
		beWrapped;
		textFont: self theme textFont;
		textColor: self theme textColor;
		backgroundColor: self theme errorPopperBackgroundColor.
	self
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		addMorph: textMorph fullFrame: (LayoutFrame identity)
]

{ #category : #initialization }
GLMErrorPopper >> withString: string from: aMorph [

	aMorph textArea removeHighlightSegment.
	super withString: string from: aMorph
]
