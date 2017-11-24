"
This presentation offers a means to embed a Morph. It makes sense only for the Morph Renderer.
"
Class {
	#name : #GLMMorphPresentation,
	#superclass : #GLMPresentation,
	#instVars : [
		'morphBuilder'
	],
	#category : #'Glamour-Presentations'
}

{ #category : #initialization }
GLMMorphPresentation >> initialize [
	super initialize.
	morphBuilder := [ :thisPresentation :theDisplayValue | theDisplayValue asMorph ]
]

{ #category : #rendering }
GLMMorphPresentation >> morph [
	^ morphBuilder glamourValue: 
			(self asGlamorousMultiValue, 
			self displayValue asGlamorousMultiValue)
]

{ #category : #rendering }
GLMMorphPresentation >> morph: aBlock [
	morphBuilder := aBlock
]

{ #category : #rendering }
GLMMorphPresentation >> renderGlamorouslyOn: aRenderer [ 
	self registerAnnouncements.
	^ aRenderer renderMorphPresentation: self
]
