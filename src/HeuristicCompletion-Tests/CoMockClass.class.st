"
I am a mock class used in the tests to get the information.
"
Class {
	#name : #CoMockClass,
	#superclass : #Object,
	#instVars : [
		'instanceVariables',
		'selectors',
		'classVariables',
		'superclass'
	],
	#category : #'HeuristicCompletion-Tests-Core'
}

{ #category : #configuration }
CoMockClass >> addInstanceVariable: aString [ 
	
	instanceVariables := instanceVariables copyWith: aString
]

{ #category : #accessing }
CoMockClass >> classVarNames [
	
	^ classVariables
]

{ #category : #accessing }
CoMockClass >> classVariables: aCollection [

	classVariables := aCollection
]

{ #category : #initialization }
CoMockClass >> initialize [

	super initialize.
	selectors := OrderedCollection new
]

{ #category : #accessing }
CoMockClass >> instVarNames [
	
	^ self instanceVariables
]

{ #category : #accessing }
CoMockClass >> instanceSide [
	
	^ self
]

{ #category : #accessing }
CoMockClass >> instanceVariables [
	^ instanceVariables
]

{ #category : #accessing }
CoMockClass >> instanceVariables: anObject [
	instanceVariables := anObject
]

{ #category : #accessing }
CoMockClass >> selectors [
	^ selectors
]

{ #category : #accessing }
CoMockClass >> selectors: aCollection [ 
	selectors := aCollection
]

{ #category : #enumerating }
CoMockClass >> selectorsDo: aBlockClosure [ 
	
	selectors do: aBlockClosure
]

{ #category : #accessing }
CoMockClass >> superclass [
	
	^ superclass
]

{ #category : #accessing }
CoMockClass >> superclass: aCompletionMockClass [ 
	
	superclass := aCompletionMockClass
]
