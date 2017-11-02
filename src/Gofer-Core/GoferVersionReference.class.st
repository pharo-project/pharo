"
A GoferVersionReference refers to a specific version of a Monticello package.
"
Class {
	#name : #GoferVersionReference,
	#superclass : #GoferReference,
	#instVars : [
		'package',
		'author',
		'branch',
		'versionNumber'
	],
	#category : 'Gofer-Core'
}

{ #category : #accessing }
GoferVersionReference >> author [
	"Answer the author of the receiver."
	
	^ author
]

{ #category : #accessing }
GoferVersionReference >> branch [
	"Answer the branch of the receiver."
	
	^ branch
]

{ #category : #initialization }
GoferVersionReference >> initializeName: aString [
	super initializeName: aString.
	self parseName: aString
]

{ #category : #private }
GoferVersionReference >> matches: aResolvedReference [
	^ self name = aResolvedReference name
]

{ #category : #accessing }
GoferVersionReference >> packageName [
	"Answer the package of the receiver."

	^ package
]

{ #category : #initialization }
GoferVersionReference >> parseName: aString [
	| basicName |
	basicName := aString last isDigit
		ifTrue: [ aString ]
		ifFalse: [ (aString copyUpToLast: $.) copyUpTo: $( ].
	package := basicName copyUpToLast: $-.
	(package includes: $.)
		ifFalse: [ branch := '' ]
		ifTrue: [
			branch := package copyAfter: $..
			package := package copyUpTo: $. ].
	author := (basicName copyAfterLast: $-) copyUpToLast: $..
	versionNumber := (basicName copyAfterLast: $-) copyAfterLast: $..
	(versionNumber notEmpty and: [ versionNumber allSatisfy: [ :each | each isDigit ] ])
		ifTrue: [ versionNumber := versionNumber asInteger ]
		ifFalse: [ versionNumber := 0 ]
]

{ #category : #accessing }
GoferVersionReference >> versionNumber [
	"Answer the version of the receiver."

	^ versionNumber
]
