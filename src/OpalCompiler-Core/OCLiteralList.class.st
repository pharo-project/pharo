"
Holds a unique ordered collection of literals.
I uses an special dictionary inside that compares literals using #literalEquals message.
I am optimised to guarantee that literals all unique and have an unique index.
"
Class {
	#name : 'OCLiteralList',
	#superclass : 'Object',
	#instVars : [
		'literalsDict',
		'first',
		'messageSends'
	],
	#category : 'OpalCompiler-Core-Extras',
	#package : 'OpalCompiler-Core',
	#tag : 'Extras'
}

{ #category : 'adding' }
OCLiteralList >> addIndexedElement: anObject to: aCollection [

	| index |
	index := self arraySize + 1.

	"I keep the first element, so it can be easily returned"
	index = 1 ifTrue: [ first := anObject ].

	"The index of the last inserted element is the the size of the literalsDict,
	as the elements are always added and never removed"
	^ aCollection at: anObject ifAbsentPut: index.
]

{ #category : 'adding' }
OCLiteralList >> addLiteral: anObject [

	^ self addIndexedElement: anObject to: literalsDict
]

{ #category : 'adding' }
OCLiteralList >> addMessageSelector: selector [

	^ self addIndexedElement: selector to: messageSends
]

{ #category : 'converting' }
OCLiteralList >> arraySize [
	^ messageSends size * 3 + literalsDict size.

]

{ #category : 'converting' }
OCLiteralList >> asArray [

	| result |

	result := Array new: self arraySize.

	literalsDict associationsDo: [ :anAssoc | result at: anAssoc value put: anAssoc key ].
	messageSends associationsDo: [ :anAssoc | result at: anAssoc value put: anAssoc key ].

	^ result
]

{ #category : 'adding' }
OCLiteralList >> first [

	^ first
]

{ #category : 'initialization' }
OCLiteralList >> initialize [

	super initialize.
	literalsDict := OCLiteralDictionary new.
	messageSends := OCLiteralDictionary new.
]

{ #category : 'testing' }
OCLiteralList >> isEmpty [

	^ literalsDict isEmpty
]

{ #category : 'accessing' }
OCLiteralList >> literalIndexOf: anElement ifAbsent: exceptionBlock [

	^ literalsDict at: anElement ifAbsent: exceptionBlock
]
