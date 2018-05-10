"
I am ZnPositionableReadStream.
I am polymorphic with (the most used/important methods of) ReadStream and PositionableStream.

I wrap another read stream and store the elements that I read in a sliding circular buffer so that I am able to go back to any position inside that buffer. 

Essentially, I implement #position and #position: to be used to back out of reading ahead.

Note that the size of my buffer limits how far I can go backwards. A SubscriptOutOfBounds exception will be signalled when an attempt is made to go too far backwards.

The index returned by #position should be considered abstract, without concrete meaning, but it is currently implemented as the count of elements read by #next on the wrapped stream. On a simple stream over an in memory collection, that will be equivalent to an integer index into that collection. But on network streams or streams that were already further along, this will no longer be the case.

The most elementary example of my capabilities can be seen in my implementation of #peek. See also the unit tests #testPlainExcursion and #testSearch

Of course, backing out of an excursion is only possible within the window of the buffer size.

Implementation

- stream <ReadStream> the read stream that I wrap and add positioning to
- buffer <String|ByteArray> sliding, circular buffer
- index <PositiveInteger> zero based index into buffer, where next will be stored
- count <PositiveInteger> number of next operations done on wrapped stream
- delta <PositiveInteger> number of positions that I was moved backwards

The real core methods are #next, #atEnd, #position and #position: and are used to implement the rest.

Part of Zinc HTTP Components
"
Class {
	#name : #ZnPositionableReadStream,
	#superclass : #Object,
	#instVars : [
		'stream',
		'buffer',
		'count',
		'index',
		'delta'
	],
	#category : #'Zinc-Character-Encoding-Core'
}

{ #category : #'instance creation' }
ZnPositionableReadStream class >> on: readStream [
	"Create an instance of ZnPositionableReadStream that wraps readStream to add limited positioning capabilities to it."
	
	^ self new
		on: readStream;
		yourself
]

{ #category : #convenience }
ZnPositionableReadStream class >> on: readStream do: block [
	"Execute block with as argument a ZnPositionableReadStream on readStream.
	Return the value of block."

	^ block value: (self on: readStream)
]

{ #category : #testing }
ZnPositionableReadStream >> atEnd [
	"Answer whether I can access any more objects."
	
	^ delta = 0 and: [ stream atEnd ]
		
]

{ #category : #positioning }
ZnPositionableReadStream >> back [
	"Go back one element and return it."

	self skip: -1.
	^ self peek
]

{ #category : #accessing }
ZnPositionableReadStream >> bufferSize [
	"Return the size of my buffer, which limits how far I can be positioned backwards. See #sizeBuffer: to set another buffer size."
	
	^ buffer size
]

{ #category : #'initialize-release' }
ZnPositionableReadStream >> close [
	"Close me after which I can no longer be accessed. I delegate this to the stream that I wrap."
	
	stream close
]

{ #category : #accessing }
ZnPositionableReadStream >> collectionSpecies [
	"Return the collection class able to hold my elements"

	^ (stream respondsTo: #collectionSpecies)
		ifTrue: [ stream collectionSpecies ]
		ifFalse: [ stream isBinary
				ifTrue: [ ByteArray ]
				ifFalse: [ String ] ]
]

{ #category : #accessing }
ZnPositionableReadStream >> defaultBufferSize [
	"Return the default size of my buffer, which limits how far I can be positioned backwards. See #sizeBuffer: to set another buffer size."
	
	^ 2 raisedToInteger: 8
]

{ #category : #'initialize-release' }
ZnPositionableReadStream >> initialize [
	super initialize.
	count := index := delta := 0
]

{ #category : #testing }
ZnPositionableReadStream >> isBinary [
	"Return whether I am binary, whether my elements are byte values (8 bit integers between 0 and 255)"
	
	^ stream isBinary
]

{ #category : #accessing }
ZnPositionableReadStream >> next [
	"Return the next element and move over it"
	
	| next |
	delta = 0
		ifTrue: [ 
			(next := stream next) ifNotNil: [ 
				count := count + 1.
				buffer at: index + 1 put: next.
				index := (index + 1) \\ buffer size ] ]
		ifFalse: [ 
			next := buffer at: ((index - delta) \\ buffer size) + 1.
			delta := delta - 1 ].
	^ next
]

{ #category : #accessing }
ZnPositionableReadStream >> next: requestedCount [ 
	"Read requestedCount elements and return them as a collection.
	If less are available, a smaller collection will be returned."

	^ self 
		next: requestedCount 
		into: (self collectionSpecies new: requestedCount)
]

{ #category : #accessing }
ZnPositionableReadStream >> next: requestedCount into: collection [
	"Read requestedCount elements into collection,
	returning a copy if less elements are available"
	
	^ self 
		next: requestedCount 
		into: collection 
		startingAt: 1   
]

{ #category : #accessing }
ZnPositionableReadStream >> next: requestedCount into: collection startingAt: offset [
	"Read requestedCount elements into collection starting at offset,
	returning a copy if less elements are available"
	
	| read |
	read := self 
		readInto: collection 
		startingAt: offset 
		count: requestedCount.
	^ read = requestedCount 
		ifTrue: [ collection ]
		ifFalse: [ collection copyFrom: 1 to: offset + read - 1 ]     
]

{ #category : #accessing }
ZnPositionableReadStream >> nextInto: collection [
	"Read the next elements of the receiver into collection,
	returning a copy if less elements are available"
	
	^ self
		next: collection size
		into: collection
]

{ #category : #'instance creation' }
ZnPositionableReadStream >> on: readStream [
	"Initialize me on readStream"
	
	stream := readStream.
	self sizeBuffer: self defaultBufferSize
]

{ #category : #accessing }
ZnPositionableReadStream >> peek [
	"Return the next element but do not move over it"
	
	^ self savingPositionDo: [ self next ]
]

{ #category : #accessing }
ZnPositionableReadStream >> peekFor: object [
	"Answer false and do not move over the next element if it is not equal to object, or if the receiver is at the end. 
	Answer true and move over the next element when it is equal to object."

	^ self peek = object
		ifTrue: [ 
			self next.
			true ]
		ifFalse: [ false ]
]

{ #category : #positioning }
ZnPositionableReadStream >> position [
	"Return my current position. This is an object that can be used as argument to #position: to move back to that position. Although opaque, it is currently an integer count of the number of #next operations done on the stream that I wrap."
	
	^ count - delta
]

{ #category : #positioning }
ZnPositionableReadStream >> position: newPosition [
	"Move my current position to newPosition, an object obtained by previously calling #position. My buffer size limits how far I can be positioned backwards. A SubscriptOutOfBounds exception will be signalled in case this operation cannot be completed. It is also no possible to go backwards unless data has been read previously."
	
	| newDelta |
	newDelta := count - newPosition.
	(newDelta between: 0 and: (buffer size min: count))
		ifFalse: [ 
			^ SubscriptOutOfBounds 
					signalFor: self 
					lowerBound: self position 
					upperBound: self position - (buffer size min: count) ].
	^ delta := newDelta
]

{ #category : #accessing }
ZnPositionableReadStream >> readInto: collection startingAt: offset count: requestedCount [
	"Read requestedCount elements into collection starting at offset,
	returning the number of elements read, there could be less elements available."

	0 to: requestedCount - 1 do: [ :count | | object |
		(object := self next) ifNil: [ ^ count ].  
		collection at: offset + count put: object ].
	^ requestedCount
]

{ #category : #positioning }
ZnPositionableReadStream >> savingPositionDo: block [
	"Execute block so that any reading from me in it has no effect afterwards. I remember the current #position and move back to it using #position: after evaluating block. My buffer size limits how long the excursion can be. A SubscriptOutOfBounds exception will be signalled in case this operation cannot be completed."
	
	| savedPosition |
	savedPosition := self position.
	^ block ensure: [ self position: savedPosition ]
]

{ #category : #'initialize-release' }
ZnPositionableReadStream >> sizeBuffer: size [
	"Change the buffer size. This should be done when I am still in my initial state."
	
	buffer := self collectionSpecies new: size
]

{ #category : #accessing }
ZnPositionableReadStream >> skip: integer [
	"Skip over integer count elements."
	
	integer > 0
		ifTrue: [ integer timesRepeat: [ self next ] ]
		ifFalse: [ self position: (self position + integer) ]
]

{ #category : #accessing }
ZnPositionableReadStream >> upTo: value [ 
	"Read upto but not including value and return them as a collection.
	If value is not found, return the entire contents of the stream."
	
	^ self collectionSpecies 
		streamContents: [ :writeStream | | element |
			[ self atEnd or: [ (element := self next) = value ] ] whileFalse: [ 
				writeStream nextPut: element ] ]
]

{ #category : #accessing }
ZnPositionableReadStream >> upToEnd [
	"Read elements until the stream is atEnd and return them as a collection."

	^ self collectionSpecies
		streamContents: [ :collectionStream | 
			[ self atEnd ] whileFalse: [ collectionStream nextPut: self next ] ]
]

{ #category : #accessing }
ZnPositionableReadStream >> wrappedStream [
	"Return the read stream that I wrap."
	
	^ stream
]
