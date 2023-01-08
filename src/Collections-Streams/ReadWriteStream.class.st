"
I represent an accessor for a sequence of objects. My instances can both read and store objects.
"
Class {
	#name : #ReadWriteStream,
	#superclass : #WriteStream,
	#category : #'Collections-Streams-Base'
}

{ #category : #comparing }
ReadWriteStream >> = other [

	(self class == ReadWriteStream and: [other class == ReadWriteStream]) ifFalse: [
		^ super = other].	"does an identity test.  Don't read contents of FileStream"
	^ self position = other position and: [self contents = other contents]
]

{ #category : #accessing }
ReadWriteStream >> contents [

	readLimit := readLimit max: position.
	^ collection copyFrom: 1 to: readLimit
]

{ #category : #comparing }
ReadWriteStream >> hash [

	self class == ReadWriteStream ifFalse: [^ super hash].
	^ (self position + readLimit + 53) hash
]

{ #category : #accessing }
ReadWriteStream >> name [
	^ 'a stream'   "for fileIn compatibility"
]

{ #category : #accessing }
ReadWriteStream >> next [
	"Primitive. Return the next object in the Stream represented by the
	receiver. Fail if the collection of this stream is not an Array or a String.
	Fail if the stream is positioned at its end, or if the position is out of
	bounds in the collection. Optional. See Object documentation
	whatIsAPrimitive."

	"treat me as a FIFO"
	position >= readLimit
		ifTrue: [^nil]
		ifFalse: [^collection at: (position := position + 1)]
]

{ #category : #accessing }
ReadWriteStream >> next: anInteger [
	"Answer the next anInteger elements of my collection.  overriden for efficiency"

	| ans endPosition |
	readLimit := readLimit max: position.

	endPosition := position + anInteger  min:  readLimit.
	ans := collection copyFrom: position+1 to: endPosition.
	position := endPosition.
	^ans
]

{ #category : #accessing }
ReadWriteStream >> readInto: aCollection startingAt: startIndex count: n [
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

{ #category : #converting }
ReadWriteStream >> readStream [
	"polymorphic with SequenceableCollection.  Return self"

	^ self
]
