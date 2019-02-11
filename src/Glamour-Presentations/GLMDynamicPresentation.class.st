"
A GLMDynamicPresentation is a presentation that takes the actual presentation from the display value. Like this, we can have the block of display return a presentation depending on the input.

A usage for this behavior is the Glamorous Editor, which takes the text as input and based on this it displays the browser.
"
Class {
	#name : #GLMDynamicPresentation,
	#superclass : #GLMPresentation,
	#instVars : [
		'presentationTransformation',
		'cachedPresentation'
	],
	#category : #'Glamour-Presentations'
}

{ #category : #rendering }
GLMDynamicPresentation >> cachedPresentation [
	"currentPresentation applies the transformation dynamically.
	This means that calling it twice, will retrieve two different objects.
	To solve the problem, cachedPresentation stores the last currentPresentation, and 
	it can be used for further inspection"
	^ cachedPresentation
]

{ #category : #rendering }
GLMDynamicPresentation >> currentPresentation [ 
	| composite |
	composite := GLMCompositePresentation new.
	self presentationTransformation glamourValue: (
			composite asGlamorousMultiValue, 
			self displayValue asGlamorousMultiValue).
	composite pane: self pane.
	composite initializePresentation.
	cachedPresentation := composite.
	^ composite
"	^ self displayValue pane: self pane"
]

{ #category : #initialization }
GLMDynamicPresentation >> initialize [ 
	super initialize.
	presentationTransformation := [ :aComposite :each | aComposite custom: each ]
]

{ #category : #events }
GLMDynamicPresentation >> outerPortEvent: aPortEvent [
	
	super outerPortEvent: aPortEvent.
	"Propage the event to cached presentations"
	self cachedPresentation ifNotNil: [ 
		self cachedPresentation outerPortEvent: aPortEvent ] 
]

{ #category : #scripting }
GLMDynamicPresentation >> presentation: aBlock [
	self presentationTransformation: aBlock
]

{ #category : #accessing }
GLMDynamicPresentation >> presentationTransformation [
	^ presentationTransformation
]

{ #category : #accessing }
GLMDynamicPresentation >> presentationTransformation: anObject [
	presentationTransformation := anObject
]

{ #category : #rendering }
GLMDynamicPresentation >> renderGlamorouslyOn: aRenderer [ 
	self registerAnnouncements.
	^ aRenderer renderDynamicPresentation: self
]

{ #category : #updating }
GLMDynamicPresentation >> updateToolbar [
	super updateToolbar.
	cachedPresentation ifNotNil: [ 
		cachedPresentation updateToolbar.
		cachedPresentation presentations do: #updateToolbar ]
]

{ #category : #scripting }
GLMDynamicPresentation >> with: aBlock [
	self presentationTransformation: aBlock
]

{ #category : #enumerating }
GLMDynamicPresentation >> withAllPanesAndPresentationsDo: aBlock [
	super withAllPanesAndPresentationsDo: aBlock.
	self cachedPresentation ifNotNil: [ :presentation | presentation withAllPanesAndPresentationsDo: aBlock ]
]
