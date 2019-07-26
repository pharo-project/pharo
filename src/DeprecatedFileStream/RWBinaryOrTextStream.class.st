"
A simulation of a FileStream, but living totally in memory.  Hold the contents of a file or web page from the network.  Can then fileIn like a normal FileStream.

Need to be able to switch between binary and text, as a FileStream does, without recopying the whole collection.  Convert to binary upon input and output.  Always keep as text internally.
"
Class {
	#name : #RWBinaryOrTextStream,
	#superclass : #ReadWriteStream,
	#instVars : [
		'isBinary'
	],
	#category : #'DeprecatedFileStream-Base'
}

{ #category : #deprecation }
RWBinaryOrTextStream class >> isDeprecated [
	^true
]

{ #category : #converting }
RWBinaryOrTextStream >> asBinaryOrTextStream [

	^ self
]

{ #category : #modes }
RWBinaryOrTextStream >> ascii [
	isBinary := false
]

{ #category : #modes }
RWBinaryOrTextStream >> binary [
	isBinary := true
]

{ #category : #accessing }
RWBinaryOrTextStream >> contents [
	"Answer with a copy of my collection from 1 to readLimit."

	| newArray |
	readLimit := readLimit max: position.
	isBinary ifFalse: [ "String"
		^ collection copyFrom: 1 to: readLimit].
	newArray := ByteArray new: readLimit.
	^ newArray replaceFrom: 1
		to: readLimit
		with: collection
		startingAt: 1.
]

{ #category : #testing }
RWBinaryOrTextStream >> isBinary [
	^ isBinary
]

{ #category : #accessing }
RWBinaryOrTextStream >> next [

	| byte |
	^ isBinary 
			ifTrue: [byte := super next.
				 byte ifNil: [nil] ifNotNil: [byte asciiValue]]
			ifFalse: [super next].

]

{ #category : #accessing }
RWBinaryOrTextStream >> next: anInteger [ 
	"Answer the next anInteger elements of my collection. Must override to get class right."

	| newArray |
	newArray := (isBinary ifTrue: [ByteArray] ifFalse: [ByteString]) new: anInteger.
	^ self nextInto: newArray
]

{ #category : #accessing }
RWBinaryOrTextStream >> next: n into: aCollection startingAt: startIndex [
	"Read n objects into the given collection. 
	Return aCollection or a partial copy if less than n elements have been read."
	"Overriden for efficiency"
	| max |
	max := (readLimit - position) min: n.
	aCollection 
		replaceFrom: startIndex 
		to: startIndex+max-1
		with: collection
		startingAt: position+1.
	position := position + max.
	max = n
		ifTrue:[^aCollection]
		ifFalse:[^aCollection copyFrom: 1 to: startIndex+max-1]
]

{ #category : #accessing }
RWBinaryOrTextStream >> next: anInteger putAll: aCollection startingAt: startIndex [
	"Optimized for ByteArrays"
	aCollection class == ByteArray 
		ifTrue:[^super next: anInteger putAll: aCollection asString startingAt: startIndex].
	^super next: anInteger putAll: aCollection startingAt: startIndex
]

{ #category : #accessing }
RWBinaryOrTextStream >> nextPut: charOrByte [
	^ super nextPut: charOrByte asCharacter
]

{ #category : #accessing }
RWBinaryOrTextStream >> nextPutAll: aCollection [
	"Optimized for ByteArrays"
	^ aCollection class == ByteArray 
		ifTrue: [ super nextPutAll: aCollection asString ]
		ifFalse: [ super nextPutAll: aCollection ]
]

{ #category : #converting }
RWBinaryOrTextStream >> on: aCollection [

	super on: aCollection.
	readLimit := 0
]

{ #category : #accessing }
RWBinaryOrTextStream >> padToEndWith: aChar [
	"We don't have pages, so we are at the end, and don't need to pad."
]

{ #category : #accessing }
RWBinaryOrTextStream >> readInto: aCollection startingAt: startIndex count: n [
	"Read n objects into the given collection. 
	Return number of elements that have been read."
	"Overriden for efficiency"
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

{ #category : #initialization }
RWBinaryOrTextStream >> reset [
	"Set the receiver's position to the beginning of the sequence of objects."

	super reset.
	isBinary ifNil: [isBinary := false].
	collection class == ByteArray ifTrue: ["Store as String and convert as needed."
		collection := collection asString.
		isBinary := true].

]

{ #category : #modes }
RWBinaryOrTextStream >> text [
	isBinary := false
]

{ #category : #accessing }
RWBinaryOrTextStream >> upTo: anObject [ 
	"Answer a subcollection from the current access position to the 
	occurrence (if any, but not inclusive) of anObject in the receiver. If 
	anObject is not in the collection, answer the entire rest of the receiver."
	| newStream element species |
	species := isBinary ifTrue:[ByteArray] ifFalse:[String].
	newStream := WriteStream on: (species new: 100).
	[self atEnd or: [(element := self next) = anObject]]
		whileFalse: [newStream nextPut: element].
	^newStream contents
]

{ #category : #accessing }
RWBinaryOrTextStream >> upToEnd [
	"Must override to get class right."
	| newArray |
	newArray := (isBinary ifTrue: [ByteArray] ifFalse: [ByteString]) new: self size - self position.
	^ self nextInto: newArray
]
