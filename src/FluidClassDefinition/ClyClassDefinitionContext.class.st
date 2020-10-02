Class {
	#name : #ClyClassDefinitionContext,
	#superclass : #ClySourceCodeContext,
	#category : #'FluidClassDefinition-Editor'
}

{ #category : #testing }
ClyClassDefinitionContext >> isClassSelected [

	^ true
]

{ #category : #testing }
ClyClassDefinitionContext >> isMessageSelected [
	"this is bad because we may want to have implementor in this pane!"
	^ false
]

{ #category : #testing }
ClyClassDefinitionContext >> isMethodSelected [

	^ false
]

{ #category : #testing }
ClyClassDefinitionContext >> selectedClass [
	
	^ self tool editingClass 

]
