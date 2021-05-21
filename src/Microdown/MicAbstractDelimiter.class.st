"
I represent a delimiter used in grammar. I'm abstract.
I'm useful for inline split parsing process.

Methods to overide:
- isCloser, isOpener: answer true or false whether am I a closing markup or an opening markup (I can be both for a format for instance, when markup is the same for both uses)
- type: my type name, as a symbol
- markup (a string)

isActive is true when I must be part of searched delimiters during normal inline split process.
(false for instance for delimiters relative to link or figure since they are analyzed in special procedure)

My index refers to the index character in global string which is parsed.

Always instantiate me with my constructor ""index:"" to precise it.
"
Class {
	#name : #MicAbstractDelimiter,
	#superclass : #Object,
	#instVars : [
		'index'
	],
	#category : #'Microdown-ModelInline'
}

{ #category : #accessing }
MicAbstractDelimiter class >> associatedInlineBlock [
	^ self subclassResponsibility
]

{ #category : #accessing }
MicAbstractDelimiter class >> index: anIndex [
	^ self new index: anIndex; yourself
]

{ #category : #testing }
MicAbstractDelimiter class >> isAbstract [

	^ self == MicAbstractDelimiter
]

{ #category : #testing }
MicAbstractDelimiter class >> isActive [
	^ true
]

{ #category : #testing }
MicAbstractDelimiter class >> isBoth [
	^ self isCloser and: self isOpener
]

{ #category : #accessing }
MicAbstractDelimiter class >> isCloser [
	^ self subclassResponsibility 
]

{ #category : #testing }
MicAbstractDelimiter class >> isCloserOnly [
	^ self isCloser and: self isOpener not
]

{ #category : #accessing }
MicAbstractDelimiter class >> isOpener [
	^ self subclassResponsibility 
]

{ #category : #testing }
MicAbstractDelimiter class >> isOpenerOnly [
	^ self isCloser not and: self isOpener
]

{ #category : #accessing }
MicAbstractDelimiter class >> markup [
	^ self subclassResponsibility 
]

{ #category : #accessing }
MicAbstractDelimiter class >> size [
	^ self markup size
]

{ #category : #accessing }
MicAbstractDelimiter class >> type [
	^ self subclassResponsibility 
]

{ #category : #accessing }
MicAbstractDelimiter >> associatedInlineBlock [
	^ self class associatedInlineBlock 
]

{ #category : #accessing }
MicAbstractDelimiter >> index [
	^ index
]

{ #category : #accessing }
MicAbstractDelimiter >> index: anInteger [
	index := anInteger
]

{ #category : #testing }
MicAbstractDelimiter >> isActive [
	^ self class isActive
]

{ #category : #testing }
MicAbstractDelimiter >> isCloser [
	^ self class isCloser
]

{ #category : #testing }
MicAbstractDelimiter >> isOpener [
	^ self class isOpener
]

{ #category : #testing }
MicAbstractDelimiter >> markup [
	^ self class markup
]

{ #category : #printing }
MicAbstractDelimiter >> printOn: aStream [
	| name |
	name := self class name.
	aStream nextPutAll: (self class name copyFrom: 4 to: name size - 'Delimiter' size ).
	aStream nextPut: $:; space ; nextPutAll: self markup
	
]

{ #category : #accessing }
MicAbstractDelimiter >> size [
	^ self class size
]

{ #category : #accessing }
MicAbstractDelimiter >> type [
	^ self class type
]
