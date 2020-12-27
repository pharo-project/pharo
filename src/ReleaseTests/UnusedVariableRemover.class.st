"
A little class to encapsulate some behavior to clean classes.
"
Class {
	#name : #UnusedVariableRemover,
	#superclass : #Object,
	#category : #'ReleaseTests-CleanCode'
}

{ #category : #analysis }
UnusedVariableRemover >> classesWithUnusedVariables [
	"self new classesWithUnusedVariables"

	^ Smalltalk globals allBehaviors 
		select: [ :each | each instanceVariables anySatisfy: [ :iv | iv isReferenced not] ]
]

{ #category : #analysis }
UnusedVariableRemover >> unusedVariables [
	"self new unusedVariables"

	^ self classesWithUnusedVariables collect: [ :each | each -> (self unusedVariablesOf: each )]
]

{ #category : #analysis }
UnusedVariableRemover >> unusedVariablesOf: aClass [
	"self new unusedVariablesOf: CDClassDefinitionNode"

	^ aClass instanceVariables select: [ :iv | iv isReferenced not]
]
