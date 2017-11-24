"
I am an argument or temporary variable of a method or block.

index is not the index in the bytecode but just used for sorting
(see tempVars). to be fixed...
"
Class {
	#name : #OCAbstractLocalVariable,
	#superclass : #OCAbstractVariable,
	#instVars : [
		'name'
	],
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #testing }
OCAbstractLocalVariable >> isLocal [

	^true
]

{ #category : #'read/write usage' }
OCAbstractLocalVariable >> isUninitialized [

	^ self isWrite not
]

{ #category : #accessing }
OCAbstractLocalVariable >> name [

	^ name
]

{ #category : #initializing }
OCAbstractLocalVariable >> name: string [
	name := string
]
