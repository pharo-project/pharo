"
This is a framework class that can be used by clients that create custom browsers containing text presentations with custom highlighters.

Usage example:

GLMRubricHighlightedTextPresentation new
	editingMode: [ 
		GLMHighlighterTextRubEditingMode withStylerDecorator: 
			GLMHighlighterTextStylerDecorator new ].
"
Class {
	#name : #GLMHighlighterTextRubEditingMode,
	#superclass : #RubEditingMode,
	#instVars : [
		'stylerDecorator'
	],
	#category : #Glamour-Rubric-Presentations
}

{ #category : #'instance creation' }
GLMHighlighterTextRubEditingMode class >> withParserStyler: aParser [

	^ self new stylerDecorator: (GLMHighlighterTextStylerDecorator
			withParserStyler: aParser)
]

{ #category : #'instance creation' }
GLMHighlighterTextRubEditingMode class >> withStylerDecorator: aDecorator [

	^ self new stylerDecorator: aDecorator
]

{ #category : #accessing }
GLMHighlighterTextRubEditingMode >> stylerDecorator [
	
	^ stylerDecorator
]

{ #category : #accessing }
GLMHighlighterTextRubEditingMode >> stylerDecorator: anObject [
	
	stylerDecorator := anObject
]

{ #category : #initialize-release }
GLMHighlighterTextRubEditingMode >> updateTextAreaWhenPlugged [
	self withStyler.
	super updateTextAreaWhenPlugged
]

{ #category : #initialize-release }
GLMHighlighterTextRubEditingMode >> updateTextAreaWhenUnplugged [
	self withoutStyler.
	super updateTextAreaWhenUnplugged


]

{ #category : #initialize-release }
GLMHighlighterTextRubEditingMode >> withStyler [
	self textArea withDecorator: self stylerDecorator
]

{ #category : #initialize-release }
GLMHighlighterTextRubEditingMode >> withoutStyler [
	self textArea withoutDecorator: self stylerDecorator
]
