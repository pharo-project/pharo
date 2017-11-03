"
I am ZnBufferedReadStream.

I wrap another ReadStream and add efficient buffering for the typical access pattern of parsers: sending lots of #next, #peek and #atEnd messages.

By design I do not implement #position and #position: or anything based on that.

I can wrap both binary or character streams and act accordingly.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnBufferedReadStream,
	#superclass : #Object,
	#instVars : [
		'stream',
		'buffer',
		'position',
		'limit'
	],
	#category : #Zinc-Character-Encoding-Core
}

{ #category : #'instance creation' }
ZnBufferedReadStream class >> on: readStream [
	^ self new
		on: readStream;
		yourself
]

{ #category : #convenience }
ZnBufferedReadStream class >> on: readStream do: block [
	"Execute block with as argument a ZnBufferedReadStream on readStream.
	Return the value of block."

	^ block value: (self on: readStream)
]

{ #category : #testing }
ZnBufferedReadStream >> atEnd [
	^ position > limit and: [ stream atEnd ]
	
]

{ #category : #initialize-release }
ZnBufferedReadStream >> close [
	stream close
]

{ #category : #accessing }
ZnBufferedReadStream >> collectionSpecies [
	^ stream isBinary
		ifTrue: [ ByteArray ]
		ifFalse: [ String ]
]

{ #category : #accessing }
ZnBufferedReadStream >> defaultBufferSize [
	^ 2 raisedToInteger: 16
]

{ #category : #initialization }
ZnBufferedReadStream >> initialize [
	super initialize.
	position := 1.
	limit := 0
]

{ #category : #accessing }
ZnBufferedReadStream >> isBinary [
	^ stream isBinary
]

{ #category : #accessing }
ZnBufferedReadStream >> next [
	"Return the next element and move over it"
	
	position > limit
		ifTrue: [ self nextBuffer ].
	^ position <= limit
		ifTrue: [ 
			| char |
			char := buffer at: position.
			position := position + 1.
			char ]
		ifFalse: [ nil ]
]

{ #category : #accessing }
ZnBufferedReadStream >> next: requestedCount [ 
	"Read requestedCount elements and return them as a collection.
	If less are available, a smaller collection will be returned."

	^ self 
		next: requestedCount 
		into: (self collectionSpecies new: requestedCount)
]

{ #category : #accessing }
ZnBufferedReadStream >> next: requestedCount into: collection [
	"Read requestedCount elements into collection,
	returning a copy if less elements are available"
	
	^ self 
		next: requestedCount 
		into: collection 
		startingAt: 1   
]

{ #category : #accessing }
ZnBufferedReadStream >> next: requestedCount into: collection startingAt: offset [
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

{ #category : #private }
ZnBufferedReadStream >> nextBuffer [
	limit := stream readInto: buffer startingAt: 1 count: buffer size.
	position := 1
]

{ #category : #accessing }
ZnBufferedReadStream >> nextInto: collection [
	"Read the next elements of the receiver into collection,
	returning a copy if less elements are available"
	
	^ self
		next: collection size
		into: collection
]

{ #category : #initialize-release }
ZnBufferedReadStream >> on: readStream [
	stream := readStream.
	self sizeBuffer: self defaultBufferSize
]

{ #category : #accessing }
ZnBufferedReadStream >> peek [
	"Return the next element but do not move over it"
	
	position > limit
		ifTrue: [ self nextBuffer ].
	^ position <= limit
		ifTrue: [ buffer at: position ]
		ifFalse: [ nil ]
]

{ #category : #accessing }
ZnBufferedReadStream >> peekFor: object [
	"Answer false and do not move over the next element if it is not equal to object, or if the receiver is at the end. 
	Answer true and move over the next element when it is equal to object."

	^ self peek = object
		ifTrue: [ 
			self next.
			true ]
		ifFalse: [ false ]
]

{ #category : #accessing }
ZnBufferedReadStream >> readInto: collection startingAt: offset count: requestedCount [
	"Read requestedCount elements into collection starting at offset,
	returning the number of elements read, there could be less elements available."

	| read |
	read := 0.
	position <= limit
		ifTrue: [ 
			read := (limit - position + 1) min: requestedCount.
			collection replaceFrom: offset to: offset + read - 1 with: buffer startingAt: position.
			position := position + read.
			read = requestedCount ifTrue: [ ^ requestedCount ] ].
	^ read
		+
			(requestedCount - read < (buffer size / 2)
				ifTrue: [ 
					stream atEnd
						ifTrue: [ 0 ]
						ifFalse: [ 
							self
								nextBuffer;
								readInto: collection startingAt: offset + read count: requestedCount - read ] ]
				ifFalse: [ stream readInto: collection startingAt: offset + read count: requestedCount - read ])
]

{ #category : #initialize-release }
ZnBufferedReadStream >> sizeBuffer: size [
	buffer := self collectionSpecies new: size
]

{ #category : #accessing }
ZnBufferedReadStream >> skip: count [
	"Skip over count elements.
	This could be further optimzed."
	
	count timesRepeat: [ self next ]
]

{ #category : #accessing }
ZnBufferedReadStream >> upTo: value [ 
	"Read upto but not including value and return them as a collection.
	If value is not found, return the entire contents of the stream.
	This could be further optimzed."
	
	^ self collectionSpecies 
		streamContents: [ :writeStream | | element |
			[ self atEnd or: [ (element := self next) = value ] ] whileFalse: [ 
				writeStream nextPut: element ] ]
]

{ #category : #accessing }
ZnBufferedReadStream >> upToEnd [
	"Read elements until the stream is atEnd and return them as a collection."

	^ self collectionSpecies
		streamContents: [ :out | 
			[ self atEnd ] whileFalse: [ 
				position > limit
					ifTrue: [ self nextBuffer ].	
				out next: limit - position + 1 putAll: buffer startingAt: position.
				position := limit + 1 ] ]
]

{ #category : #accessing }
ZnBufferedReadStream >> wrappedStream [
	^ stream
]
