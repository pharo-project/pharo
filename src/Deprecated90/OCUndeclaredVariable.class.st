"
I am a Variable that is not declared.

I am like a global an association, but instead of Smalltalk globals, I live in the Undeclared global dictionary.
"
Class {
	#name : #OCUndeclaredVariable,
	#superclass : #OCAbstractVariable,
	#category : #'Deprecated90-OpalCompiler-Core'
}

{ #category : #testing }
OCUndeclaredVariable class >> isDeprecated [
	"Use UndeclaredVariable directly instead"
	^true
]

{ #category : #accessing }
OCUndeclaredVariable class >> semanticNodeClass [
	^RBVariableNode
]

{ #category : #emitting }
OCUndeclaredVariable >> emitStore: methodBuilder [

	methodBuilder storeIntoLiteralVariable: self variable


]

{ #category : #emitting }
OCUndeclaredVariable >> emitValue: methodBuilder [

	methodBuilder pushLiteralVariable: self variable

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
