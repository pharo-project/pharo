"
RBLiteralToken is the first-class representation of a literal token (entire literals, even literal arrays, are a single token in the Pharo grammar).

Instance Variables:
- stopPosition <Integer> The position within the source code where the token terminates.


"
Class {
	#name : 'OCLiteralToken',
	#superclass : 'OCValueToken',
	#instVars : [
		'stopPosition',
		'source'
	],
	#category : 'AST-Core-Tokens',
	#package : 'AST-Core',
	#tag : 'Tokens'
}

{ #category : 'instance creation' }
OCLiteralToken class >> value: anObject [
	| literal |
	literal := anObject class == Array
				ifTrue: [anObject collect: [:each | self value: each]]
				ifFalse: [anObject].
	^self
		value: literal
		start: nil
		stop: nil
]

{ #category : 'instance creation' }
OCLiteralToken class >> value: aString start: anInteger stop: stopInteger [
	^ self value: aString start: anInteger stop: stopInteger source: nil
]

{ #category : 'instance creation' }
OCLiteralToken class >> value: aString start: anInteger stop: stopInteger source: sourceText [
	^(self new)
		value: aString
			start: anInteger
			stop: stopInteger;
			source: sourceText;
		yourself
]

{ #category : 'testing' }
OCLiteralToken >> isLiteralToken [
	^true
]

{ #category : 'private' }
OCLiteralToken >> length [
	^stopPosition - self start + 1
]

{ #category : 'accessing' }
OCLiteralToken >> realValue [
	^value
]

{ #category : 'accessing' }
OCLiteralToken >> source [
	^source
]

{ #category : 'initialization' }
OCLiteralToken >> source: aString [
	source := aString
]

{ #category : 'accessing' }
OCLiteralToken >> stop: anObject [
	stopPosition := anObject
]

{ #category : 'storing' }
OCLiteralToken >> storeOn: aStream [
	value isSymbol
		ifTrue:
			[aStream nextPut: $#.
			((OCScanner isSelector: value) and: [value ~~ #'||'])
				ifTrue: [aStream nextPutAll: value]
				ifFalse: [value asString printOn: aStream].
			^self].
	value class == Character
		ifTrue:
			[aStream
				nextPut: $$;
				nextPut: value.
			^self].
	value storeOn: aStream
]

{ #category : 'initialization' }
OCLiteralToken >> value: aString start: anInteger stop: stopInteger [
	value := aString.
	sourcePointer := anInteger.
	stopPosition := stopInteger
]
