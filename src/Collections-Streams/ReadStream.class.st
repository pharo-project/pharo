"
I represent an accessor for a sequence of objects that can only read objects from the sequence.
"
Class {
	#name : #ReadStream,
	#superclass : #PositionableStream,
	#category : #'Collections-Streams-Base'
}

{ #category : #modes }
ReadStream >> ascii [
]

{ #category : #accessing }
ReadStream >> localName [
	^'ReadStream'
]

{ #category : #accessing }
ReadStream >> next [
	"Primitive. Answer the next object in the Stream represented by the
	receiver. Fail if the collection of this stream is not an Array or a String.
	Fail if the stream is positioned at its end, or if the position is out of
	bounds in the collection. Optional. See Object documentation
	whatIsAPrimitive."

	^ position >= readLimit
		ifTrue: [ nil ]
		ifFalse: [ collection at: (position := position + 1) ]
]

{ #category : #accessing }
ReadStream >> next: anInteger [
	"Answer the next anInteger elements of my collection.  overriden for efficiency"

	| ans endPosition |

	endPosition := position + anInteger  min:  readLimit.
	ans := collection copyFrom: position+1 to: endPosition.
	position := endPosition.
	^ans
]

{ #category : #accessing }
ReadStream >> nextPut: anObject [
	self shouldNotImplement
]

{ #category : #initialization }
ReadStream >> on: aCollection from: firstIndex to: lastIndex [

	| len |
	collection := aCollection.
	readLimit :=  lastIndex > (len := collection size)
						ifTrue: [len]
						ifFalse: [lastIndex].
	position := firstIndex <= 1
				ifTrue: [0]
				ifFalse: [firstIndex - 1]
]

{ #category : #accessing }
ReadStream >> readInto: aCollection startingAt: startIndex count: n [
	"Read n objects into the given collection.
	Return number of elements that have been read."

	| max |
	max := (readLimit - position) min: n.
	aCollection
		replaceFrom: startIndex
		to: startIndex + max - 1
		with: collection
		startingAt: position + 1.
	position := position + max.
	^ max
]

{ #category : #accessing }
ReadStream >> readStream [
	"polymorphic with SequenceableCollection.  Return self"

	^ self
]

{ #category : #accessing }
ReadStream >> size [
	"Compatibility with other streams (e.g., FileStream)"
	^readLimit
]

{ #category : #accessing }
ReadStream >> skipTo: anObject [
	"fast version using indexOf:"
	| end |
	end := collection indexOf: anObject startingAt: position+1 ifAbsent: [ 0 ].

	^(end = 0 or: [end > readLimit])
		ifTrue: [ self setToEnd. false ]
		ifFalse: [ position := end. true]
]

{ #category : #accessing }
ReadStream >> upTo: anObject [
	"fast version using indexOf:"
	| start end |

	start := position+1.
	end := collection indexOf: anObject startingAt: start ifAbsent: [ 0 ].

	"not present--return rest of the collection"
	(end = 0 or: [end > readLimit]) ifTrue: [ ^self upToEnd ].

	"skip to the end and return the data passed over"
	position := end.
	^collection copyFrom: start to: (end-1)
]

{ #category : #accessing }
ReadStream >> upToAnyOf: aCollection do: aBlock [
	"Overriden for speed"
	| end result |
	end := collection indexOfAnyOf: aCollection startingAt: 1 + position ifAbsent: [0].
	(end = 0 or: [end > readLimit]) ifTrue: [^self upToEnd].
	result := collection copyFrom: 1 + position to: -1 + end.
	position := end.
	aBlock value: (collection at: end).
	^result
]

{ #category : #accessing }
ReadStream >> upToEnd [
	| start |
	start := 1 + position.
	position := readLimit.
	^collection copyFrom: start to: position
]
