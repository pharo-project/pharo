"
A GLMMorphicAction is special GLMAction that is only active when rendering Glamour browsers with Morphic.
"
Class {
	#name : #GLMMorphicAction,
	#superclass : #GLMAction,
	#category : #'Glamour-Morphic-Renderer'
}

{ #category : #testing }
GLMMorphicAction >> worksWithMorphic [
	^ true
]
