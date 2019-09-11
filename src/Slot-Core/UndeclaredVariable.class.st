"
I model undeclared variable bindings. I am stored as a literal inside methods which reference me.

The compiler forwards bytecode generation to me for accessing the variable. 

In future I can profice logging and user warning when evaluated code accesses undeclared variables
"
Class {
	#name : #UndeclaredVariable,
	#superclass : #LiteralVariable,
	#category : #'Slot-Core-Variables'
}

{ #category : #queries }
UndeclaredVariable >> definingClass [
	"Nobody defines undeclared variable"
	^nil
]

{ #category : #'code generation' }
UndeclaredVariable >> emitStore: methodBuilder [

	methodBuilder storeIntoLiteralVariable: self
]

{ #category : #'code generation' }
UndeclaredVariable >> emitValue: methodBuilder [
	methodBuilder pushLiteralVariable: self
]

{ #category : #testing }
UndeclaredVariable >> isReferenced [
 	"A Undeclared could be referenced from any method"
 	^Smalltalk globals allBehaviors
 		anySatisfy: [ :behavior | behavior hasSelectorReferringTo: self ]
]
