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
OCUndeclaredVariable class >> semanticNodeKind [

	^RBUndefinedKind
]

{ #category : #emitting }
OCUndeclaredVariable >> emitStore: methodBuilder [

	methodBuilder storeIntoLiteralVariable: self variable


]

{ #category : #emitting }
OCUndeclaredVariable >> emitValue: methodBuilder [

	methodBuilder pushLiteralVariable: self variable

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

{ #category : #accessing }
OCUndeclaredVariable >> variable [
	^Undeclared associationAt: name
]
