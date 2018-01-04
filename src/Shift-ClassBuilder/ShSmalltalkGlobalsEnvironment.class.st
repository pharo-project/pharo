"
I represent the Smalltalk globals environment. 
I am used to generate the classes by the ShiftClassBuilder when it is using directly the Smalltalk environment.
"
Class {
	#name : #ShSmalltalkGlobalsEnvironment,
	#superclass : #ShGlobalsEnvironment,
	#category : 'Shift-ClassBuilder'
}

{ #category : #accessing }
ShSmalltalkGlobalsEnvironment >> classNamed: aName [
	^ environment at: aName asSymbol ifAbsent:[nil].
]

{ #category : #initialization }
ShSmalltalkGlobalsEnvironment >> initialize [
	super initialize.
	self environment: Smalltalk globals.
]
