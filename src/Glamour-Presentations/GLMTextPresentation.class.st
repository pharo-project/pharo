"
A presentation displaying text.

Instance Variables:
	selectedTextBlock	<ProtoObject>
	highlightSmalltalk	<Boolean>
	highlightSmalltalkContext	<Object>
	textBlock	<ProtoObject | PseudoContext>
"
Class {
	#name : #GLMTextPresentation,
	#superclass : #GLMTextualPresentation,
	#instVars : [
		'highlightSmalltalk',
		'highlightSmalltalkContext'
	],
	#category : #'Glamour-Presentations'
}

{ #category : #accessing }
GLMTextPresentation >> forSmalltalk [
	highlightSmalltalk := true
]

{ #category : #accessing }
GLMTextPresentation >> forSmalltalk: aBlock [
	"aBlock takes as parameters the entity objects and 
	its evaluation is expected to produce a Smalltalk class"
	highlightSmalltalk := true.
	highlightSmalltalkContext := aBlock.
]

{ #category : #accessing }
GLMTextPresentation >> highlightSmalltalk [
	^ highlightSmalltalk ifNil: [highlightSmalltalk := false]
]

{ #category : #accessing }
GLMTextPresentation >> highlightSmalltalkContext [

	^ highlightSmalltalkContext
		ifNil: [ nil ]
		ifNotNil: [ highlightSmalltalkContext glamourValue: self entity ]
]

{ #category : #rendering }
GLMTextPresentation >> renderGlamorouslyOn: aRenderer [ 
	self deprecated:  'Use GLMRubricTextPresentation instead.'.
	self registerAnnouncements.
	^ aRenderer renderTextPresentation: self
]
