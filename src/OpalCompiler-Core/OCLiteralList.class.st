"
Holds a unique ordered collection of literals.
I uses an special dictionary inside that compares literals using #literalEquals message.
I am optimised to guarantee that literals all unique and have an unique index.
"
Class {
	#name : #OCLiteralList,
	#superclass : #Object,
	#instVars : [
		'literalsDict',
		'lastIndex',
		'first'
	],
	#category : #'OpalCompiler-Core-Extras'
}

{ #category : #adding }
OCLiteralList >> addLiteral: anObject [

	"I keep the first element, so it can be easily returned"
	lastIndex = 0 
		ifTrue: [ first := anObject ].
	
	^ literalsDict at: anObject ifAbsentPut: [ lastIndex := lastIndex + 1 ]
]

{ #category : #converting }
OCLiteralList >> asArray [

	| result |
	
	result := Array new: lastIndex.
	
	literalsDict associationsDo: [ :anAssoc | result at: anAssoc value put: anAssoc key ].
	
	^ result.
]

{ #category : #adding }
OCLiteralList >> first [
	
	^ first 
]

{ #category : #initialization }
OCLiteralList >> initialize [

	super initialize.
	lastIndex := 0.
	literalsDict := OCLiteralDictionary new
]

{ #category : #testing }
OCLiteralList >> isEmpty [

	^ lastIndex = 0
]

{ #category : #accessing }
OCLiteralList >> literalIndexOf: anElement ifAbsent: exceptionBlock [

	^ literalsDict at: anElement ifAbsent: exceptionBlock
]
