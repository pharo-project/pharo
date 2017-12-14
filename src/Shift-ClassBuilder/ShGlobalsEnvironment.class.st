"
I wrap an environment used by the ShiftClassBuilder. 
In this way the environment can be anything in the system. 
"
Class {
	#name : #ShGlobalsEnvironment,
	#superclass : #Object,
	#instVars : [
		'environment'
	],
	#category : 'Shift-ClassBuilder'
}

{ #category : #accessing }
ShGlobalsEnvironment >> classNamed: aName [
	^ environment classNamed:aName.
]

{ #category : #accessing }
ShGlobalsEnvironment >> environment [
	^ environment
]

{ #category : #accessing }
ShGlobalsEnvironment >> environment: anObject [
	environment := anObject
]

{ #category : #testing }
ShGlobalsEnvironment >> hasClassNamed: aName [
	^ environment hasClassNamed: aName
]
