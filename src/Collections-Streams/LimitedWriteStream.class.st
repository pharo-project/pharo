"
A LimitedWriteStream is a specialized WriteStream that has a maximum size of the collection it streams over. When this limit is reached a special limitBlock is executed. This can for example be used to ""bail out"" of lengthy streaming operations before they have finished.  For a simple example take a look at the universal Object printString.

The message SequenceableCollection class streamContents:limitedTo: creates a LimitedWriteStream. In this case it prevents very large (or possibly recursive) object structures to ""overdo"" their textual representation. 
"
Class {
	#name : #LimitedWriteStream,
	#superclass : #WriteStream,
	#instVars : [
		'limit',
		'limitBlock'
	],
	#category : #'Collections-Streams-Base'
}

{ #category : #accessing }
LimitedWriteStream class >> defaultLimit [
	^ 1000
]

{ #category : #'instance creation' }
LimitedWriteStream class >> on: aCollection [
	^ (self basicNew on: aCollection)
		setLimit: self defaultLimit
		limitBlock: nil
]

{ #category : #'instance creation' }
LimitedWriteStream class >> on: aCollection from: firstIndex to: lastIndex [
	^ (self basicNew on: aCollection from: firstIndex to: lastIndex)
		setLimit: self defaultLimit
		limitBlock: nil
]

{ #category : #'instance creation' }
LimitedWriteStream class >> on: aCollection limit: anInteger limitBlock: aLimitBlock [
	^ (self basicNew on: aCollection)
		setLimit: anInteger
		limitBlock: aLimitBlock
]

{ #category : #'instance creation' }
LimitedWriteStream class >> with: aCollection [
	^ (self basicNew with: aCollection)
		setLimit: self defaultLimit
		limitBlock: nil
]

{ #category : #'instance creation' }
LimitedWriteStream class >> with: aCollection from: firstIndex to: lastIndex [
	^ (self basicNew with: aCollection from: firstIndex to: lastIndex)
		setLimit: self defaultLimit
		limitBlock: nil
]

{ #category : #accessing }
LimitedWriteStream >> limit [
	^ limit
]

{ #category : #accessing }
LimitedWriteStream >> limit: anInteger [
	limit := anInteger.

	position > limit
		ifTrue: [
			position := limit.
			limitBlock value ]
]

{ #category : #accessing }
LimitedWriteStream >> limitBlock [
	^ limitBlock
]

{ #category : #accessing }
LimitedWriteStream >> limitBlock: aBlock [
	limitBlock := aBlock
]

{ #category : #accessing }
LimitedWriteStream >> nextPut: anObject [
	"Ensure that the limit is not exceeded"

	position >= limit
		ifTrue: [ limitBlock value ]
		ifFalse: [ super nextPut: anObject ].
	^ anObject
]

{ #category : #accessing }
LimitedWriteStream >> nextPutAll: aCollection [

	| newEnd |
	collection class == aCollection class
		ifFalse: [ ^ super nextPutAll: aCollection ].

	newEnd := position + aCollection size.
	newEnd > limit ifTrue: [
		super nextPutAll: (aCollection copyFrom: 1 to: (limit - position max: 0)).
		limitBlock value.
		^ aCollection
	].
	newEnd > writeLimit ifTrue: [
		self growTo: newEnd + 10
	].

	collection replaceFrom: position+1 to: newEnd  with: aCollection startingAt: 1.
	position := newEnd.

	^ aCollection
]

{ #category : #private }
LimitedWriteStream >> pastEndPut: anObject [
	collection size >= limit ifTrue: [limitBlock value].  "Exceptional return"
	^ super pastEndPut: anObject
]

{ #category : #initialization }
LimitedWriteStream >> setLimit: anInteger limitBlock: aBlock [
	"Limit the numer of elements this stream will write..."
	limit := anInteger.

	"Execute this (typically ^ contents) when that limit is exceded"
	limitBlock := aBlock.

	"To support legacy users, this test must be here too"
	position > limit
		ifTrue: [
			position := limit.
			limitBlock value ]
]
