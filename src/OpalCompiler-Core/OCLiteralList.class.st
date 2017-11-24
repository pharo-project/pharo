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

{ #category : #private }
OCLiteralList >> setCollection: anArray [

	super setCollection: anArray.
	equalitySet := OCLiteralSet new: anArray size.

]
