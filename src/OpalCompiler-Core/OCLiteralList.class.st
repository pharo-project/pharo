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
		'first'
	],
	#category : 'OpalCompiler-Core-DataStructures',
	#package : 'OpalCompiler-Core',
	#tag : 'DataStructures'
}

{ #category : 'adding' }
OCLiteralList >> addLiteral: anObject [

	"I keep the first element, so it can be easily returned"
	literalsDict ifEmpty: [ first := anObject ].

	"The index of the last inserted element is the the size of the literalsDict,
	as the elements are always added and never removed"
	^ literalsDict at: anObject ifAbsentPut: [ literalsDict size + 1 ]
]

{ #category : 'converting' }
OCLiteralList >> asArray [

	| result |

	result := Array new: literalsDict size.

	literalsDict associationsDo: [ :anAssoc | result at: anAssoc value put: anAssoc key ].

	^ result
]

{ #category : 'adding' }
OCLiteralList >> first [

	^ first
]

{ #category : 'initialization' }
OCLiteralList >> initialize [

	super initialize.
	literalsDict := OCLiteralDictionary new
]

{ #category : 'testing' }
OCLiteralList >> isEmpty [

	^ literalsDict isEmpty
]

{ #category : 'accessing' }
OCLiteralList >> literalIndexOf: anElement ifAbsent: exceptionBlock [

	^ literalsDict at: anElement ifAbsent: exceptionBlock
]
