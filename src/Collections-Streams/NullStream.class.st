"
I am a stream that does nothing i.e. generates and consumes an infinite number of elements (NullObject pattern). I can be used as an equivalent of /dev/null (for example 
a logging in off mode) or for performance benchmarks.
"
Class {
	#name : #NullStream,
	#superclass : #Stream,
	#instVars : [
		'binary',
		'position',
		'lastElement'
	],
	#category : #'Collections-Streams-Base'
}

{ #category : #'instance creation' }
NullStream class >> new [
	"Creates a new instance"

	^self basicNew initialize
]

{ #category : #modes }
NullStream >> ascii [
	"Switches the stream to ascii mode"

	binary := false
]

{ #category : #testing }
NullStream >> atEnd [
	"Answer whether the receiver can access any more objects."

	^false
]

{ #category : #modes }
NullStream >> binary [
	"Switches the stream to binary mode"

	binary := true
]

{ #category : #accessing }
NullStream >> collectionSpecies [
	"The type of collection returned by the stream"

	^binary ifTrue: [ByteArray] ifFalse: [ByteString]
]

{ #category : #accessing }
NullStream >> contents [
	"Answer all of the contents of the receiver."

	self shouldNotImplement
]

{ #category : #writing }
NullStream >> cr [
	^ self nextPut: Character cr
]

{ #category : #accessing }
NullStream >> element [
	"The element returned by the stream"

	^binary ifTrue: [0] ifFalse: [Character value: 0]
]

{ #category : #initialization }
NullStream >> initialize [
	"Initialize the receiver"

	binary := false.
	position := 0
]

{ #category : #testing }
NullStream >> isBinary [
	"Return true if the receiver is a binary byte stream"

	^binary
]

{ #category : #testing }
NullStream >> isEmpty [
	"Answer whether the receiver's contents has no elements."

	^false
]

{ #category : #writing }
NullStream >> lf [
	^ self nextPut: Character lf
]

{ #category : #accessing }
NullStream >> next [
	"Answer the next object accessible by the receiver."

	position := position + 1.
	^self element
]

{ #category : #accessing }
NullStream >> next: anInteger [
	"Answer the next anInteger elements of my collection. Must override
	because default uses self contents species, which might involve a large
	collection."

	position := position + anInteger.
	^self collectionSpecies new: anInteger
]

{ #category : #reading }
NullStream >> next: n into: aCollection [
	"Read n objects into the given collection.
	Return aCollection or a partial copy if less than
	n elements have been read."

	^self next: n into: aCollection startingAt: 1
]

{ #category : #reading }
NullStream >> next: n into: aCollection startingAt: startIndex [
	"Read n objects into the given collection.
	Return aCollection or a partial copy if less than
	n elements have been read."

	position := position + n.
	^aCollection
]

{ #category : #writing }
NullStream >> next: anInteger putAll: aCollection [
	"Store the next anInteger elements from the given collection."

	^self next: anInteger putAll: aCollection startingAt: 1
]

{ #category : #accessing }
NullStream >> next: anInteger putAll: aCollection startingAt: startIndex [
	"Store the next anInteger elements from the given collection."

	position := position + anInteger.
	lastElement := aCollection at: anInteger + startIndex - 1.
	^aCollection
]

{ #category : #reading }
NullStream >> nextInto: aCollection [
	"Read the next elements of the receiver into aCollection.
	Return aCollection or a partial copy if less than aCollection
	size elements have been read."

	^self next: aCollection size into: aCollection startingAt: 1
]

{ #category : #reading }
NullStream >> nextInto: aCollection startingAt: startIndex [
	"Read the next elements of the receiver into aCollection.
	Return aCollection or a partial copy if less than aCollection
	size elements have been read."

	^self next: (aCollection size - startIndex +1) into: aCollection startingAt: startIndex
]

{ #category : #accessing }
NullStream >> nextPut: anObject [
	"Insert the argument, anObject, as the next object accessible by the
	receiver. Answer anObject."

	position := position + 1.
	lastElement := anObject.
	^anObject
]

{ #category : #accessing }
NullStream >> nextPutAll: aCollection [
	"Append the elements of aCollection to the sequence of objects accessible
	by the receiver. Answer aCollection."

	position := position + aCollection size.
	aCollection isEmpty
		ifFalse: [ lastElement := aCollection last ].
	^ aCollection
]

{ #category : #writing }
NullStream >> peek [
	"Answer what would be returned if the message next were sent to the
	receiver. If the receiver is at the end, answer nil."

	^self element
]

{ #category : #reading }
NullStream >> peekLast [
	^ lastElement
]

{ #category : #positioning }
NullStream >> position [
	"Answer the current position of accessing the sequence of objects."

	^position
]

{ #category : #positioning }
NullStream >> position: anInteger [
	"Set the current position for accessing the objects to be anInteger, as long
	as anInteger is within the bounds of the receiver's contents. If it is not,
	create an error notification."

	(anInteger >= 0)
		ifTrue: [position := anInteger]
		ifFalse: [self positionError]
]

{ #category : #positioning }
NullStream >> positionError [
	"Since I am not necessarily writable, it is up to my subclasses to override
	position: if expanding the collection is preferrable to giving this error."

	self error: 'Attempt to set the position of a PositionableStream out of bounds'
]

{ #category : #printing }
NullStream >> printOn: aStream [
	aStream nextPutAll: 'a '; nextPutAll: self class name
]

{ #category : #reading }
NullStream >> readInto: collection startingAt: startIndex count: n [
	"Read n objects into the given collection.
	Return number of elements that have been read."

	position := position + n.
	^ n
]

{ #category : #initialization }
NullStream >> reset [
	"Set the receiver's position to the beginning of the sequence of objects."

	position := 0
]

{ #category : #positioning }
NullStream >> skip: anInteger [
	"Set the receiver's position to be the current position+anInteger. A
	subclass might choose to be more helpful and select the minimum of the
	receiver's size and position+anInteger, or the maximum of 1 and
	position+anInteger for the repositioning."

	self position: position + anInteger
]

{ #category : #writing }
NullStream >> space [
	^ self nextPut: Character space
]

{ #category : #writing }
NullStream >> tab [
	^ self nextPut: Character tab
]
