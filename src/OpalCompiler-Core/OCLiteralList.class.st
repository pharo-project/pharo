"
Holds a unique ordered collection of literals
"
Class {
	#name : #OCLiteralList,
	#superclass : #OrderedCollection,
	#instVars : [
		'equalitySet'
	],
	#category : #'OpalCompiler-Core-Extras'
}

{ #category : #adding }
OCLiteralList >> addLast: object [
	"Only add if not already in list"

	(equalitySet includes: object) ifTrue: [^ object].
	equalitySet add: object.
	super addLast: object.
	^ object

]

{ #category : #accessing }
OCLiteralList >> indexOf: anElement startingAt: start ifAbsent: exceptionBlock [

	start to: self size do:
		[:index | ((self at: index) literalEqual: anElement) ifTrue: [^ index]].
	^ exceptionBlock value
]

{ #category : #accessing }
OCLiteralList >> literalIndexOf: anElement ifAbsent: exceptionBlock [
	"Answer the index of anElement within the receiver. If the receiver does 
	not contain anElement, answer the result of evaluating the argument, 
	exceptionBlock."
	1 to: self size do:
		[:i | ((self at: i) literalEqual: anElement) ifTrue: [^ i]].
	^ exceptionBlock value
]

{ #category : #private }
OCLiteralList >> setCollection: anArray [

	super setCollection: anArray.
	equalitySet := OCLiteralSet new: anArray size.

]
