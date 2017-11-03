Class {
	#name : #GLMTabbedArrangement,
	#superclass : #GLMCompositeArrangement,
	#category : #Glamour-Core
}

{ #category : #rendering }
GLMTabbedArrangement >> renderGlamorouslyOn: aRenderer [
	^ aRenderer renderTabbedCompositePresentation: self composite
]
