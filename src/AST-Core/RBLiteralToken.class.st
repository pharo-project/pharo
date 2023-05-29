"
RBLiteralToken is the first-class representation of a literal token (entire literals, even literal arrays, are a single token in the Pharo grammar).

Instance Variables:
- stopPosition <Integer> The position within the source code where the token terminates.


"
Class {
	#name : #RBLiteralToken,
	#superclass : #RBValueToken,
	#instVars : [
		'stopPosition',
		'source'
	],
	#category : #'AST-Core-Tokens'
}

{ #category : #'instance creation' }
RBLiteralToken class >> value: anObject [
	| literal |
	literal := anObject class == Array
				ifTrue: [anObject collect: [:each | self value: each]]
				ifFalse: [anObject].
	^self
		value: literal
		start: nil
		stop: nil
]

{ #category : #'instance creation' }
RBLiteralToken class >> value: aString start: anInteger stop: stopInteger [
	^ self value: aString start: anInteger stop: stopInteger source: nil
]

{ #category : #'instance creation' }
RBLiteralToken class >> value: aString start: anInteger stop: stopInteger source: sourceText [
	^(self new)
		value: aString
			start: anInteger
			stop: stopInteger;
			source: sourceText;
		yourself
]

{ #category : #testing }
RBLiteralToken >> isLiteralToken [
	^true
]

{ #category : #testing }
RBLiteralToken >> isMultiKeyword [
	^false
]

{ #category : #private }
RBLiteralToken >> length [
	^stopPosition - self start + 1
]

{ #category : #accessing }
RBLiteralToken >> realValue [
	^value
]

{ #category : #accessing }
RBLiteralToken >> source [
	^source
]

{ #category : #initialization }
RBLiteralToken >> source: aString [
	source := aString
]

{ #category : #accessing }
RBLiteralToken >> stop: anObject [
	stopPosition := anObject
]

{ #category : #printing }
RBLiteralToken >> storeOn: aStream [
	value isSymbol
		ifTrue:
			[aStream nextPut: $#.
			(value isValidSelector and: [value ~~ #'||'])
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

{ #category : #initialization }
RBLiteralToken >> value: aString start: anInteger stop: stopInteger [
	value := aString.
	sourcePointer := anInteger.
	stopPosition := stopInteger
]
