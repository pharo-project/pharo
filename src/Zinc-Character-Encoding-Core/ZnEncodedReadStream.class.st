"
I am ZnEncodedReadStream, an abstract support class for read streams on a binary encoded wrapped stream.

Part of Zinc HTTP Components
"
Class {
	#name : #ZnEncodedReadStream,
	#superclass : #ZnEncodedStream,
	#instVars : [
		'peeked'
	],
	#category : #'Zinc-Character-Encoding-Core'
}

{ #category : #testing }
ZnEncodedReadStream >> atEnd [
	^ peeked isNil and: [ stream atEnd ]
]

{ #category : #accessing }
ZnEncodedReadStream >> contents [
	"This is technically not correct, but it is better than nothing"
	
	^ self upToEnd
]

{ #category : #testing }
ZnEncodedReadStream >> isBinary [
	^ false
]

{ #category : #accessing }
ZnEncodedReadStream >> next [
	^ peeked
		ifNil: [ 
			stream atEnd ifFalse: [ self nextElement ] ]
		ifNotNil: [ | character |
			character := peeked.
			peeked := nil.
			character ]
]

{ #category : #accessing }
ZnEncodedReadStream >> next: requestedCount [ 
	"Read requestedCount elements into new collection and return it,
	 it could be that less elements were available"

	^ self 
		next: requestedCount 
		into: (self collectionSpecies new: requestedCount)
]

{ #category : #accessing }
ZnEncodedReadStream >> next: requestedCount into: collection [
	"Read requestedCount elements into collection,
	returning a copy if less elements are available"
	
	^ self
		next: requestedCount
		into: collection
		startingAt: 1
]

{ #category : #accessing }
ZnEncodedReadStream >> next: requestedCount into: collection startingAt: offset [
	"Read requestedCount elements into collection starting at offset,
	returning a copy if less elements are available"
	
	| readCount |
	readCount := self 
		readInto: collection 
		startingAt: offset 
		count: requestedCount.
	^ requestedCount = readCount
		ifTrue: [ collection ]
		ifFalse: [ collection copyFrom: 1 to: offset + readCount - 1 ]
]

{ #category : #private }
ZnEncodedReadStream >> nextElement [
	self subclassResponsibility 
]

{ #category : #accessing }
ZnEncodedReadStream >> peek [
	^ peeked
		ifNil: [
			stream atEnd ifFalse: [ peeked := self nextElement ] ]
]

{ #category : #accessing }
ZnEncodedReadStream >> peekFor: object [
	^ self peek = object
		ifTrue: [ 
			self next.
			true ]
		ifFalse: [ false ]
]

{ #category : #accessing }
ZnEncodedReadStream >> readInto: collection startingAt: offset count: requestedCount [
	"Read requestedCount elements into collection starting at offset,
	returning the number of elements read, there could be less elements available.
	This is an inefficient abstract implementation, reading one by one."
	
	0 to: requestedCount - 1 do: [ :count | | object |
		(object := self next) ifNil: [ ^ count ].  
		collection at: offset + count put: object ].
	^ requestedCount
]

{ #category : #accessing }
ZnEncodedReadStream >> skip: count [
	count timesRepeat: [ self next ]
]

{ #category : #accessing }
ZnEncodedReadStream >> upTo: anObject [ 	
	^ self collectionSpecies 
		streamContents: [ :out | | element |
			[ self atEnd or: [ (element := self next) = anObject ] ] whileFalse: [ 
				out nextPut: element ] ]
]

{ #category : #accessing }
ZnEncodedReadStream >> upToEnd [
	^ self collectionSpecies
		streamContents: [ :collectionStream | 
			[ self atEnd ] whileFalse: [ collectionStream nextPut: self next ] ]
]
