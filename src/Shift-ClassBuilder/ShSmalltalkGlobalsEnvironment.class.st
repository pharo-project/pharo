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
ShSmalltalkGlobalsEnvironment >> bindingAt: aString [

	^ environment bindingOf: aString
]

{ #category : #accessing }
ShSmalltalkGlobalsEnvironment >> classNamed: aName [
	^ environment at: aName asSymbol ifAbsent: [ nil ]
]

{ #category : #accessing }
ShSmalltalkGlobalsEnvironment >> classNamed: aString ifAbsent: aBlockClosure [

	^ environment at: aString ifAbsent: aBlockClosure
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

{ #category : #installation }
ShSmalltalkGlobalsEnvironment >> installClass: aClass withName: aString [

	environment at: aString put: aClass
]

{ #category : #accessing }
ShSmalltalkGlobalsEnvironment >> organization [

	^ environment organization
]
