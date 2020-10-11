Class {
	#name : #ClyClassDefinitionContext,
	#superclass : #ClySourceCodeContext,
	#category : #'Calypso-SystemTools-Core-Editors-Classes'
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
