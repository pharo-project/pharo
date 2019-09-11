"
I model Globals. I am stored as a binding in the Smalltalk globals.

The compiler forwards bytecode generation to me for accessing the variable.
"
Class {
	#name : #GlobalVariable,
	#superclass : #LiteralVariable,
	#category : #'Slot-Core-Variables'
}

{ #category : #queries }
GlobalVariable >> definingClass [
	"The class defining the variable. For Globals, return nil"
	^nil
]

{ #category : #'code generation' }
GlobalVariable >> emitStore: methodBuilder [
	methodBuilder storeIntoLiteralVariable: self.


]

{ #category : #'code generation' }
GlobalVariable >> emitValue: methodBuilder [
	methodBuilder pushLiteralVariable: self.
]

{ #category : #testing }
GlobalVariable >> isGlobalVariable [
	^true
]

{ #category : #testing }
GlobalVariable >> isReferenced [
 	"A Global could be referenced from any method"
 	^Smalltalk globals allBehaviors
 		anySatisfy: [ :behavior | behavior hasSelectorReferringTo: self ]
]
