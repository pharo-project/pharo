Class {
	#name : #GLMTabbedArrangement,
	#superclass : #GLMCompositeArrangement,
	#category : #'Glamour-Core'
}

{ #category : #testing }
GLMTabbedArrangement class >> isDeprecated [
	^ true
]

{ #category : #rendering }
GLMTabbedArrangement >> renderGlamorouslyOn: aRenderer [
	^ aRenderer renderTabbedCompositePresentation: self composite
]
