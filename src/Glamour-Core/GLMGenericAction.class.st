"
This is an action that is supposed to work in any context (e.g., Morphic or Seaside)
"
Class {
	#name : #GLMGenericAction,
	#superclass : #GLMAction,
	#category : 'Glamour-Core'
}

{ #category : #testing }
GLMGenericAction >> worksWithMorphic [
	^ true
]

{ #category : #testing }
GLMGenericAction >> worksWithSeaside [
	^ true
]
