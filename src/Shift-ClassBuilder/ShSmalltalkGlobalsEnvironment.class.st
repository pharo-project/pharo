"
I represent the Smalltalk globals environment. 
I am used to generate the classes by the ShiftClassBuilder when it is using directly the Smalltalk environment.
"
Class {
	#name : #ShSmalltalkGlobalsEnvironment,
	#superclass : #Object,
	#instVars : [
		'environment'
	],
	#category : #'Shift-ClassBuilder'
}

{ #category : #accessing }
ShSmalltalkGlobalsEnvironment >> at: aName ifAbsent: aBlock [

	^ environment at: aName ifAbsent: aBlock
]

{ #category : #installation }
ShSmalltalkGlobalsEnvironment >> at: aString put: aClass [

	environment at: aString put: aClass
]

{ #category : #accessing }
ShSmalltalkGlobalsEnvironment >> bindingOf: aString [

	^ environment bindingOf: aString
]

{ #category : #accessing }
ShSmalltalkGlobalsEnvironment >> environment [
	^ environment
]

{ #category : #accessing }
ShSmalltalkGlobalsEnvironment >> environment: anObject [
	environment := anObject
]

{ #category : #testing }
ShSmalltalkGlobalsEnvironment >> hasClassNamed: aName [
	^ environment hasClassNamed: aName
]

{ #category : #initialization }
ShSmalltalkGlobalsEnvironment >> initialize [
	super initialize.
	self environment: Smalltalk globals
]

{ #category : #accessing }
ShSmalltalkGlobalsEnvironment >> organization [

	^ environment organization
]
