"
I am a Variable that is not declared.

I am like a global an association, but instead of Smalltalk globals, I live in the Undeclared global dictionary.
"
Class {
	#name : #OCUndeclaredVariable,
	#superclass : #OCAbstractVariable,
	#instVars : [
		'name'
	],
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #accessing }
OCUndeclaredVariable class >> semanticNodeClass [
	^RBVariableNode
]

{ #category : #accessing }
OCUndeclaredVariable >> assoc [
	^Undeclared associationAt: name
]

{ #category : #emitting }
OCUndeclaredVariable >> emitStore: methodBuilder [

	methodBuilder storeIntoLiteralVariable:  self assoc


]

{ #category : #emitting }
OCUndeclaredVariable >> emitValue: methodBuilder [

	methodBuilder pushLiteralVariable: self assoc.

]

{ #category : #testing }
OCUndeclaredVariable >> isUndeclared [

	^ true
]

{ #category : #accessing }
OCUndeclaredVariable >> name [
	^ name
]

{ #category : #accessing }
OCUndeclaredVariable >> name: anObject [
	name := anObject
]
