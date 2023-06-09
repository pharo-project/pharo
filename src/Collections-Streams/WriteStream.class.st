"
I represent an accessor for a sequence of objects that can only store objects in the sequence.
"
Class {
	#name : #WriteStream,
	#superclass : #PositionableStream,
	#instVars : [
		'writeLimit'
	],
	#category : #'Collections-Streams-Base'
}

{ #category : #'instance creation' }
WriteStream class >> with: aCollection [
	"Answer an instance of me on the argument, aCollection, positioned to
	store objects at the end of aCollection."

	^self basicNew with: aCollection
]

{ #category : #'instance creation' }
WriteStream class >> with: aCollection from: firstIndex to: lastIndex [
	"Answer an instance of me on the subcollection of the argument,
	aCollection, determined by the indices firstIndex and lastIndex. Position
	the instance to store at the end of the subcollection."

	^self basicNew
		with: aCollection
		from: firstIndex
		to: lastIndex
]

{ #category : #accessing }
WriteStream >> << anObject [
	"Write anObject to the receiver, dispatching using #putOn:
	This is a shortcut for both nextPut: and nextPutAll: since anObject can be both
	the element type of the receiver as well as a collection of those elements.
	No further conversions of anObject are applied.
	This is an optimisation.
	Return self to accomodate chaining."

 	anObject class == collection class
		ifTrue: [ self nextPutAll: anObject ]
		ifFalse: [ anObject putOn: self ]
]

{ #category : #private }
WriteStream >> braceArray [
	"This method is used in compilation of brace constructs.
	It MUST NOT be deleted or altered."

	^ collection
]

{ #category : #private }
WriteStream >> braceArray: anArray [
	"This method is used in compilation of brace constructs.
	It MUST NOT be deleted or altered."

	collection := anArray.
	position := 0.
	readLimit := 0.
	writeLimit := anArray size
]

{ #category : #accessing }
WriteStream >> contents [

	readLimit := readLimit max: position.
	^collection copyFrom: 1 to: position
]

{ #category : #'character writing' }
WriteStream >> cr [
	"Append a return character to the receiver."

	self nextPut: Character cr
]

{ #category : #accessing }
WriteStream >> crlf [
	"Append a line feed character to the receiver."

	self cr; lf
]

{ #category : #'character writing' }
WriteStream >> crtab [
	"Append a return character, followed by a single tab character, to the
	receiver."

	self cr; tab
]

{ #category : #'character writing' }
WriteStream >> crtab: anInteger [
	"Append a return character, followed by anInteger tab characters, to the
	receiver."

	self cr; tab: anInteger
]

{ #category : #'character writing' }
WriteStream >> ensureASpace [
	"Append a space character to the receiver IFF there is not one on the end."

	(position > 0 and: [(collection at: position) = Character space]) ifTrue: [^self].
	self nextPut: Character space
]

{ #category : #'character writing' }
WriteStream >> ensureCr [
	"Append a cr character to the receiver IFF there is not one on the end."

	self ensureEndsWith: Character cr
]

{ #category : #accessing }
WriteStream >> ensureEndsWith: anObject [
	"Append anObject to the receiver IFF it is non-empty and there is not one on the end."

	(position == 0 or: [(collection at: position) = anObject]) ifTrue: [^self].
	self nextPut: anObject
]

{ #category : #'character writing' }
WriteStream >> ensureNoSpace [
	"If there is not one on the end, remove it."
	| space |

	space := Character space.

	[ position > 0 and: [(collection at: position) = space ]]
		whileTrue: [ self skip: -1 ]
]

{ #category : #private }
WriteStream >> growTo: anInteger [

    " anInteger is the required minimal new size of the collection "
 	| oldSize length |
 	oldSize := collection size.
      length := anInteger + (oldSize // 4 max: 20) - oldSize.
 	collection := collection grownBy: length.
 	writeLimit := collection size
]

{ #category : #'character writing' }
WriteStream >> lf [
	"Append a line feed character to the receiver."

	self nextPut: Character lf
]

{ #category : #accessing }
WriteStream >> next [

	self shouldNotImplement
]

{ #category : #accessing }
WriteStream >> next: anInteger putAll: aCollection startingAt: startIndex [
	"Store the next anInteger elements from the given collection."

	| newEnd numPut |
	collection class == aCollection class ifFalse:
		[^ super next: anInteger putAll: aCollection startingAt: startIndex ].

	numPut := anInteger min: (aCollection size - startIndex + 1).
	newEnd := position + numPut.
	newEnd > writeLimit ifTrue:
		[^ super next: anInteger putAll: aCollection startingAt: startIndex "Trigger normal pastEndPut: logic"].

	collection replaceFrom: position+1 to: newEnd with: aCollection startingAt: startIndex.
	position := newEnd
]

{ #category : #accessing }
WriteStream >> nextPut: anObject [
	"Primitive. Insert the argument at the next position in the Stream
	represented by the receiver. Fail if the collection of this stream is not an
	Array or a String. Fail if the stream is positioned at its end, or if the
	position is out of bounds in the collection. Fail if the argument is not
	of the right type for the collection. Optional. See Object documentation
	whatIsAPrimitive."

	^ position >= writeLimit
		ifTrue: [ self pastEndPut: anObject ]
		ifFalse: [ position := position + 1.
			collection at: position put: anObject ]
]

{ #category : #accessing }
WriteStream >> nextPutAll: aCollection [

 	| newEnd |
 	collection class == aCollection class ifFalse:
 		[^ super nextPutAll: aCollection ].

 	newEnd := position + aCollection size.
 	newEnd > writeLimit ifTrue:
 		[self growTo: newEnd + 10].

 	collection replaceFrom: position+1 to: newEnd  with: aCollection startingAt: 1.
 	position := newEnd.

	^ aCollection
]

{ #category : #initialization }
WriteStream >> on: aCollection [

	super on: aCollection.
	readLimit := 0.
	writeLimit := aCollection size
]

{ #category : #initialization }
WriteStream >> on: aCollection from: firstIndex to: lastIndex [

	| len |
	collection := aCollection.
	readLimit :=
		writeLimit := lastIndex > (len := collection size)
						ifTrue: [len]
						ifFalse: [lastIndex].
	position := firstIndex <= 1
				ifTrue: [0]
				ifFalse: [firstIndex - 1]
]

{ #category : #private }
WriteStream >> pastEndPut: anObject [
	"Grow the collection, then put <anObject> at the current write position."

	collection := collection grownBy: (collection size max: 20).
	writeLimit := collection size.
	collection at: (position := position + 1) put: anObject.
	^ anObject
]

{ #category : #accessing }
WriteStream >> peekLast [
	"Return that item just put at the end of the stream"

	^ position > 0
		ifTrue: [collection at: position]
		ifFalse: [nil]
]

{ #category : #positioning }
WriteStream >> position: anInteger [
	"Refer to the comment in PositionableStream|position:."

	readLimit := readLimit max: position.
	super position: anInteger
]

{ #category : #initialization }
WriteStream >> reset [
	"Refer to the comment in PositionableStream|reset."

	readLimit := readLimit max: position.
	position := 0
]

{ #category : #positioning }
WriteStream >> resetToStart [
	readLimit := position := 0
]

{ #category : #positioning }
WriteStream >> setToEnd [
	"Refer to the comment in PositionableStream|setToEnd."

	readLimit := readLimit max: position.
	super setToEnd
]

{ #category : #accessing }
WriteStream >> size [

	^readLimit := readLimit max: position
]

{ #category : #'character writing' }
WriteStream >> space [
	"Append a space character to the receiver."

	self nextPut: Character space
]

{ #category : #'character writing' }
WriteStream >> space: anInteger [
	"Append anInteger space characters to the receiver."

	self next: anInteger put: Character space
]

{ #category : #printing }
WriteStream >> store: anObject [
	"Have anObject print on the receiver for purposes of rereading."

	anObject storeOn: self
]

{ #category : #'character writing' }
WriteStream >> tab [
	"Append a tab character to the receiver."

	self nextPut: Character tab
]

{ #category : #'character writing' }
WriteStream >> tab: anInteger [
	"Append anInteger tab characters to the receiver."

	self next: anInteger put: Character tab
]

{ #category : #initialization }
WriteStream >> with: aCollection [

	super on: aCollection.
	position := readLimit := writeLimit := aCollection size
]

{ #category : #initialization }
WriteStream >> with: aCollection from: firstIndex to: lastIndex [
	self with: (aCollection copyFrom: firstIndex to: lastIndex)
]

{ #category : #private }
WriteStream >> withAttribute: att do: strmBlock [
	"No-op here is overriden in TextStream for font emphasis"
	^ strmBlock value
]

{ #category : #private }
WriteStream >> withAttributes: attributes do: strmBlock [
	"No-op here is overriden in TextStream for font emphasis"
	^ strmBlock value
]
