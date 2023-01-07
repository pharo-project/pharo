"
I represent an accessor for a sequence of objects (a collection) that are externally named by indices so that the point of access can be repositioned. I am abstract in that I do not implement the messages next and nextPut: which are inherited from my superclass Stream.
"
Class {
	#name : #PositionableStream,
	#superclass : #Stream,
	#instVars : [
		'collection',
		'position',
		'readLimit'
	],
	#category : #'Collections-Streams-Base'
}

{ #category : #testing }
PositionableStream class >> isAbstract [

	^ self == PositionableStream
]

{ #category : #'instance creation' }
PositionableStream class >> on: aCollection [
	"Answer an instance of me, streaming over the elements of aCollection."

	^self basicNew on: aCollection
]

{ #category : #'instance creation' }
PositionableStream class >> on: aCollection from: firstIndex to: lastIndex [
	"Answer an instance of me on a copy of the argument, aCollection,
	determined by the indices firstIndex and lastIndex. Position the instance
	at the beginning of the collection."

	^self basicNew
		on: aCollection
		from: firstIndex
		to: lastIndex
]

{ #category : #enumerating }
PositionableStream class >> with: aCollectionOrStream do: aBlock [
	"Evaluates a block with a new stream based on the collection or stream. Answers the result of the block evaluation. Follows the style of FileStream>>fileNamed:do:."
	| aStream |
	aStream := self on: aCollectionOrStream.
	[ ^ aBlock value: aStream ] ensure: [ aStream close ]
]

{ #category : #testing }
PositionableStream >> atEnd [
	"Primitive. Answer whether the receiver can access any more objects.
	Optional. See Object documentation whatIsAPrimitive."

	^position >= readLimit
]

{ #category : #accessing }
PositionableStream >> back [
	"Go back one element and return it."

	self position = 0 ifTrue: [self positionError].
	self skip: -1.
	^ self peek
]

{ #category : #positioning }
PositionableStream >> backUpTo: subCollection [
	"Back up the position to he subCollection.  Position must be somewhere within the stream initially.  Leave it just after it.  Return true if succeeded.  No wildcards, and case does matter."
	"Example:
	| strm | strm := ReadStream on: 'zabc abdc'.
	strm setToEnd; backUpTo: 'abc'; position
"
	| pattern startMatch |
	pattern := subCollection reversed readStream.
	startMatch := nil.
	[ pattern atEnd ] whileFalse:
		[ self position = 0 ifTrue: [ ^ false ].
		self skip: -1.
		self next = pattern next
			ifTrue: [ pattern position = 1 ifTrue: [ startMatch := self position ] ]
			ifFalse:
				[ pattern position: 0.
				startMatch ifNotNil:
					[ self position: startMatch - 1.
					startMatch := nil ] ].
		self skip: -1 ].
	self position: startMatch.
	^ true
]

{ #category : #'accessing - data' }
PositionableStream >> boolean [
	"Answer the next boolean value from this (binary) stream."

	^ self next ~= 0
]

{ #category : #'accessing - data' }
PositionableStream >> boolean: aBoolean [
	"Store the given boolean value on this (binary) stream."

	self nextPut: (aBoolean ifTrue: [1] ifFalse: [0])
]

{ #category : #private }
PositionableStream >> collectionSpecies [
	"Answer the species of collection into which the receiver can stream"

	^collection species
]

{ #category : #accessing }
PositionableStream >> contents [
	"Answer with a copy of my collection from 1 to readLimit."

	^collection copyFrom: 1 to: readLimit
]

{ #category : #accessing }
PositionableStream >> contentsOfEntireFile [
	"For non-file streams"
	^ self contents
]

{ #category : #'accessing - data' }
PositionableStream >> int16 [
	"Answer the next signed, 16-bit integer from this (binary) stream."

	| n |
	n := self next.
	n := (n bitShift: 8) + (self next).
	n >= 16r8000 ifTrue: [n := n - 16r10000].
	^ n
]

{ #category : #'accessing - data' }
PositionableStream >> int16: anInteger [
	"Store the given signed, 16-bit integer on this (binary) stream."

	| n |
	(anInteger < -16r8000 or: [ anInteger >= 16r8000 ])
		ifTrue: [ self error: 'outside 16-bit integer range' ].
	n := anInteger < 0
		ifTrue: [ 16r10000 + anInteger ]
		ifFalse: [ anInteger ].
	self nextPut: (n byteAt: 2).
	self nextPut: (n byteAt: 1)
]

{ #category : #'accessing - data' }
PositionableStream >> int32 [
	"Answer the next signed, 32-bit integer from this (binary) stream."
	"Details: As a fast check for negative number, check the high bit of the first digit"

	| n firstDigit |
	n := firstDigit := self next.
	n := (n bitShift: 8) + self next.
	n := (n bitShift: 8) + self next.
	n := (n bitShift: 8) + self next.
	firstDigit >= 128 ifTrue: [n := -16r100000000 + n].  "decode negative 32-bit integer"
	^ n
]

{ #category : #'accessing - data' }
PositionableStream >> int32: anInteger [
	"Store the given signed, 32-bit integer on this (binary) stream."

	| n |
	(anInteger < -16r80000000 or: [ anInteger >= 16r80000000 ])
		ifTrue: [ self error: 'outside 32-bit integer range' ].
	n := anInteger < 0
		ifTrue: [ 16r100000000 + anInteger ]
		ifFalse: [ anInteger ].
	self nextPut: (n byteAt: 4).
	self nextPut: (n byteAt: 3).
	self nextPut: (n byteAt: 2).
	self nextPut: (n byteAt: 1)
]

{ #category : #testing }
PositionableStream >> isBinary [
	"Return true if the receiver is a binary byte stream"
	^collection class == ByteArray
]

{ #category : #testing }
PositionableStream >> isEmpty [
	"Answer whether the receiver's contents has no elements."

	"Returns true if both the set of past and future sequence values of
the receiver are empty. Otherwise returns false"

	^ self atEnd and: [position = 0]
]

{ #category : #positioning }
PositionableStream >> match: subCollection [
	"Set the access position of the receiver to be past the next occurrence of the subCollection. Answer whether subCollection is found.  No wildcards, and case does matter."
	| pattern |
	pattern := subCollection readStream.
	[ self atEnd or: [pattern atEnd] ] whileFalse:
		[self skip: pattern position negated.
		pattern setToStart.
		(self skipTo: pattern next) ifFalse: [ ^false ].
		[pattern atEnd not and: [self next = pattern peek]] whileTrue: [pattern next]
	].
	^ pattern atEnd
]

{ #category : #accessing }
PositionableStream >> next: anInteger [
	"Answer the next anInteger elements of my collection. Must override
	because default uses self contents species, which might involve a large
	collection."

	| newArray |
	newArray := collection species new: anInteger.
	1 to: anInteger do: [:index | newArray at: index put: self next].
	^newArray
]

{ #category : #accessing }
PositionableStream >> next: n into: aCollection [
	"Read n objects into the given collection.
	Return aCollection or a partial copy if less than
	n elements have been read."
	^self next: n into: aCollection startingAt: 1
]

{ #category : #accessing }
PositionableStream >> next: requestedCount into: aCollection startingAt: startIndex [
	"Read requestedCount objects into the given collection.
	Return aCollection or a partial copy if less elements have been read."

	| readCount |
	readCount := self readInto: aCollection startingAt: startIndex count: requestedCount.
	^ readCount = requestedCount
		ifTrue: [ aCollection ]
		ifFalse: [ aCollection copyFrom: 1 to: startIndex + readCount - 1 ]
]

{ #category : #accessing }
PositionableStream >> next: anInteger putAll: aCollection [
	"Store the next anInteger elements from the given collection."
	^self next: anInteger putAll: aCollection startingAt: 1
]

{ #category : #accessing }
PositionableStream >> nextDelimited: terminator [
	"Answer the contents of the receiver, up to the next terminator character. Doubled terminators indicate an embedded terminator character.  For example: 'this '' was a quote'. Start postioned before the initial terminator."

	| out ch |
	out := (String new: 1000) writeStream.
	self atEnd ifTrue: [^ ''].
	self next == terminator ifFalse: [self skip: -1].	"absorb initial terminator"
	[(ch := self next) == nil] whileFalse: [
		(ch == terminator) ifTrue: [
			self peek == terminator ifTrue: [
				self next.  "skip doubled terminator"
			] ifFalse: [
				^ out contents  "terminator is not doubled; we're done!"
			].
		].
		out nextPut: ch.
	].
	^ out contents
]

{ #category : #'accessing - nonhomogeneous' }
PositionableStream >> nextInt32 [
	"Read a 32-bit signed integer from the next 4 bytes"

	| s |
	s := 0.
	1 to: 4 do: [ :i | s := (s bitShift: 8) + self next ].
	^ (s bitAnd: 16r80000000) = 0
		ifTrue: [ s ]
		ifFalse: [ -1 - s bitInvert32 ]
]

{ #category : #'accessing - nonhomogeneous' }
PositionableStream >> nextInt32Put: int32 [
	"Write a signed integer to the next 4 bytes"
	| pos |
	pos := int32 < 0
		ifTrue: [(0-int32) bitInvert32 + 1]
		ifFalse: [int32].
	1 to: 4 do: [:i | self nextPut: (pos byteAt: 5-i)].
	^ int32
]

{ #category : #accessing }
PositionableStream >> nextInto: aCollection [
	"Read the next elements of the receiver into aCollection.
	Return aCollection or a partial copy if less than aCollection
	size elements have been read."
	^self next: aCollection size into: aCollection startingAt: 1
]

{ #category : #accessing }
PositionableStream >> nextInto: aCollection startingAt: startIndex [
	"Read the next elements of the receiver into aCollection.
	Return aCollection or a partial copy if less than aCollection
	size elements have been read."
	^self next: (aCollection size - startIndex+1) into: aCollection startingAt: startIndex
]

{ #category : #accessing }
PositionableStream >> nextLine [
	"Answer next line (may be empty) without line end delimiters, or nil if at end.
	Let the stream positioned after the line delimiter(s).
	Handle a zoo of line delimiters CR, LF, or CR-LF pair"

	self atEnd ifTrue: [^nil].
	^self upToAnyOf: CharacterSet crlf do: [:char | char = Character cr ifTrue: [self peekFor: Character lf]]
]

{ #category : #'accessing - nonhomogeneous' }
PositionableStream >> nextLittleEndianNumber: n [
	"Answer the next n bytes as a positive Integer or LargePositiveInteger, where the bytes are ordered from least significant to most significant."

	| bytes s |
	bytes := self next: n.
	s := 0.
	n to: 1 by: -1 do: [:i | s := (s bitShift: 8) bitOr: (bytes at: i)].
	^ s
]

{ #category : #'accessing - nonhomogeneous' }
PositionableStream >> nextLittleEndianNumber: n put: value [
	"Answer the next n bytes as a positive Integer or LargePositiveInteger, where the bytes are ordered from least significant to most significant."
	| bytes |
	bytes := ByteArray new: n.
	1 to: n do: [:i | bytes at: i put: (value byteAt: i)].
	self nextPutAll: bytes
]

{ #category : #'accessing - nonhomogeneous' }
PositionableStream >> nextNumber: n [
	"Answer the next n bytes as a positive Integer or LargePositiveInteger."
	| s |
	s := 0.
	1 to: n do:
		[:i | s := (s bitShift: 8) bitOr: self next asInteger].
	^ s normalize
]

{ #category : #'accessing - nonhomogeneous' }
PositionableStream >> nextNumber: n put: v [
	"Append to the receiver the argument, v, which is a positive
	SmallInteger or a LargePositiveInteger, as the next n bytes.
	Possibly pad with leading zeros."

	1 to: n do: [:i | self nextPut: (v byteAt: n+1-i)].
	^ v
]

{ #category : #'accessing - nonhomogeneous' }
PositionableStream >> nextString [
	"Read a string from the receiver. The first byte is the length of the string, unless it is greater than 192, in which case the first four bytes encode the length.  I expect to be in ascii mode when called (caller puts back to binary)."

	| length aByteArray |

	"read the length in binary mode"
	self binary.
	length := self next.		"first byte."
	length >= 192 ifTrue: [length := length - 192.
		1 to: 3 do: [:ii | length := length * 256 + self next]].
	aByteArray := ByteArray new: length.

	self nextInto: aByteArray.
	^aByteArray asString
]

{ #category : #'accessing - nonhomogeneous' }
PositionableStream >> nextStringOld [
	"Read a string from the receiver. The first byte is the length of the
	string, unless it is greater than 192, in which case the first *two* bytes
	encode the length.  Max size 16K. "

	| aString length |
	length := self next.		"first byte."
	length >= 192 ifTrue: [length := (length - 192) * 256 + self next].
	aString := String new: length.
	1 to: length do: [:ii | aString at: ii put: self next asCharacter].
	^aString
]

{ #category : #'accessing - nonhomogeneous' }
PositionableStream >> nextStringPut: s [
	"Append the string, s, to the receiver.  Only used by DataStream.  Max size of 64*256*256*256."

	| length |
	(length := s size) < 192
		ifTrue: [self nextPut: length]
		ifFalse:
			[self nextPut: (length byteAt: 4)+192.
			self nextPut: (length byteAt: 3).
			self nextPut: (length byteAt: 2).
			self nextPut: (length byteAt: 1)].
	self nextPutAll: s asByteArray.
	^s
]

{ #category : #'accessing - nonhomogeneous' }
PositionableStream >> nextWord [
	"Answer the next two bytes from the receiver as an Integer."

	| high low |
	high := self next.
		high==nil ifTrue: [^false].
	low := self next.
		low==nil ifTrue: [^false].
	^(high asInteger bitShift: 8) + low asInteger
]

{ #category : #'accessing - nonhomogeneous' }
PositionableStream >> nextWordPut: aWord [
	"Append to the receiver an Integer as the next two bytes."

	self nextPut: ((aWord bitShift: -8) bitAnd: 255).
	self nextPut: (aWord bitAnd: 255).
	^aWord
]

{ #category : #accessing }
PositionableStream >> oldBack [
	"Go back one element and return it.  Use indirect messages in case I am a StandardFileStream"
	"The method is a misconception about what a stream is. A stream contains a pointer *between* elements with past and future elements. This method considers that the pointer is *on* an element. Please consider unit tests which verifies #back and #oldBack behavior. (Damien Cassou - 1 August 2007)"
	self position = 0 ifTrue: [self positionError].
	self position = 1 ifTrue: [self position: 0.  ^ nil].
	self skip: -2.
	^ self next
]

{ #category : #accessing }
PositionableStream >> oldPeekBack [
	"Return the element at the previous position, without changing position.  Use indirect messages in case self is a StandardFileStream."
	"The method is a misconception about what a stream is. A stream contains a pointer *between* elements with past and future elements. This method considers that the pointer is *on* an element. Please consider unit tests which verifies #peekBack and #oldPeekBack behavior. (Damien Cassou - 1 August 2007)"
	| element |
	element := self oldBack.
	self skip: 1.
	^ element
]

{ #category : #initialization }
PositionableStream >> on: aCollection [

	collection := aCollection.
	readLimit := aCollection size.
	position := 0.
	self reset
]

{ #category : #initialization }
PositionableStream >> on: aCollection from: firstIndex to: lastIndex [
	self on: (aCollection copyFrom: firstIndex to: lastIndex)
]

{ #category : #accessing }
PositionableStream >> originalContents [
	"Answer the receiver's actual contents collection, NOT a copy."

	^ collection
]

{ #category : #positioning }
PositionableStream >> padTo: nBytes put: aCharacter [
	"Pad using the argument, aCharacter, to the next boundary of nBytes characters."
	| rem |
	rem := nBytes - (self position \\ nBytes).
	rem = nBytes ifTrue: [^ 0].
	self next: rem put: aCharacter
]

{ #category : #positioning }
PositionableStream >> padToNextLongPut: char [
	"Make position be on long word boundary, writing the padding
	character, char, if necessary."
	[self position \\ 4 = 0]
		whileFalse: [self nextPut: char]
]

{ #category : #accessing }
PositionableStream >> peek [
	"Answer what would be returned if the message next were sent to the
	receiver. If the receiver is at the end, answer nil."

	| nextObject |
	self atEnd ifTrue: [^nil].
	nextObject := self next.
	position := position - 1.
	^nextObject
]

{ #category : #accessing }
PositionableStream >> peekBack [
	"Return the element at the previous position, without changing position.  Use indirect messages in case self is a StandardFileStream."

	| element |
	element := self back.
	self skip: 1.
	^ element
]

{ #category : #accessing }
PositionableStream >> peekFor: anObject [
	"Answer false and do not move over the next element if it is not equal to
	the argument, anObject, or if the receiver is at the end. Answer true
	and increment the position for accessing elements, if the next element is
	equal to anObject."

	| nextObject |
	self atEnd ifTrue: [^false].
	nextObject := self next.
	"peek for matching element"
	anObject = nextObject ifTrue: [^true].
	"gobble it if found"
	position := position - 1.
	^false
]

{ #category : #positioning }
PositionableStream >> position [
	"Answer the current position of accessing the sequence of objects."

	^position
]

{ #category : #positioning }
PositionableStream >> position: anInteger [
	"Set the current position for accessing the objects to be anInteger, as long
	as anInteger is within the bounds of the receiver's contents. If it is not,
	create an error notification."

	(anInteger between: 0 and: readLimit)
		ifTrue: [ position := anInteger ]
		ifFalse: [ self positionError ]
]

{ #category : #private }
PositionableStream >> positionError [
	"Since I am not necessarily writable, it is up to my subclasses to override
	position: if expanding the collection is preferrable to giving this error."

	self error: 'Attempt to set the position of a PositionableStream out of bounds'
]

{ #category : #positioning }
PositionableStream >> positionOfSubCollection: subCollection [
	"Return a position such that that element at the new position equals the first element of sub, and the next elements equal the rest of the elements of sub. Begin the search at the current position.
	If no such match is found, answer 0."

	^self positionOfSubCollection: subCollection ifAbsent: [0]
]

{ #category : #positioning }
PositionableStream >> positionOfSubCollection: subCollection ifAbsent: exceptionBlock [
	"Return a position such that that element at the new position equals the first element of sub, and the next elements equal the rest of the elements of sub. Begin the search at the current position.
	If no such match is found, answer the result of evaluating argument, exceptionBlock."
	| pattern startPosition currentPosition |
	pattern := subCollection readStream.
	startPosition := self position.
	[ pattern atEnd ] whileFalse:
		[ self atEnd ifTrue: [ ^ exceptionBlock value ].
		self next = pattern next ifFalse:
			[ self position: self position - pattern position + 1.
			pattern reset ] ].
	currentPosition := self position.
	self position: startPosition.
	^ pattern atEnd
		ifTrue: [ currentPosition + 1 - subCollection size ]
		ifFalse: [ exceptionBlock value ]
]

{ #category : #positioning }
PositionableStream >> pushBack: aString [
	"Compatibility with SocketStreams"
	self skip: aString size negated
]

{ #category : #accessing }
PositionableStream >> readInto: aCollection startingAt: startIndex count: n [
	"Read n objects into the given collection.
	Return number of elements that have been read."

	| obj |
	0 to: n - 1 do: [ :i |
		(obj := self next) == nil ifTrue: [ ^ i ].
		aCollection at: startIndex + i put: obj ].
	^ n
]

{ #category : #initialization }
PositionableStream >> reset [
	"Set the receiver's position to the beginning of the sequence of objects."

	position := 0
]

{ #category : #positioning }
PositionableStream >> resetContents [
	"Set the position and limits to 0."

	position := 0.
	readLimit := 0
]

{ #category : #private }
PositionableStream >> setFrom: newStart to: newStop [

	position := newStart - 1.
	readLimit := newStop
]

{ #category : #positioning }
PositionableStream >> setToEnd [
	"Set the position of the receiver to the end of the sequence of objects."

	position := readLimit
]

{ #category : #positioning }
PositionableStream >> setToStart [
	"Set the position of the receiver to the start of the sequence of objects."

	self reset
]

{ #category : #positioning }
PositionableStream >> skip: anInteger [
	"Set the receiver's position to be the current position+anInteger. A
	subclass might choose to be more helpful and select the minimum of the
	receiver's size and position+anInteger, or the maximum of 1 and
	position+anInteger for the repositioning."

	self position: position + anInteger
]

{ #category : #positioning }
PositionableStream >> skipSeparators [
	[self atEnd]
		whileFalse:
		[self next isSeparator ifFalse: [^ self position: self position-1]]
]

{ #category : #accessing }
PositionableStream >> skipSeparatorsAndPeekNext [
	"A special function to make nextChunk fast"
	| peek |
	[self atEnd]
		whileFalse:
		[(peek := self next) isSeparator
			ifFalse: [self position: self position-1. ^ peek]]
]

{ #category : #positioning }
PositionableStream >> skipTo: anObject [
	"Set the access position of the receiver to be past the next occurrence of
	anObject. Answer whether anObject is found."

	[self atEnd]
		whileFalse: [self next = anObject ifTrue: [^true]].
	^false
]

{ #category : #'accessing - data' }
PositionableStream >> string [
	"Answer the next string from this (binary) stream."

	| size |
	size := self uint16.
	^ (self next: size) asString
]

{ #category : #'accessing - data' }
PositionableStream >> string: aString [
	"Store the given string on this (binary) stream. The string must contain 65535 or fewer characters."

	aString size > 16rFFFF ifTrue: [self error: 'string too long for this format'].
	self uint16: aString size.
	self nextPutAll: aString asByteArray
]

{ #category : #'accessing - data' }
PositionableStream >> uint16 [
	"Answer the next unsigned, 16-bit integer from this (binary) stream."

	| n |
	n := self next.
	n := (n bitShift: 8) + (self next).
	^ n
]

{ #category : #'accessing - data' }
PositionableStream >> uint16: anInteger [
	"Store the given unsigned, 16-bit integer on this (binary) stream."

	(anInteger < 0 or: [ anInteger >= 16r10000 ])
		ifTrue: [self error: 'outside unsigned 16-bit integer range'].

	self nextPut: (anInteger byteAt: 2).
	self nextPut: (anInteger byteAt: 1)
]

{ #category : #'accessing - data' }
PositionableStream >> uint24 [
	"Answer the next unsigned, 24-bit integer from this (binary) stream."

	| n |
	n := self next.
	n := (n bitShift: 8) + self next.
	n := (n bitShift: 8) + self next.
	^ n
]

{ #category : #'accessing - data' }
PositionableStream >> uint24: anInteger [
	"Store the given unsigned, 24-bit integer on this (binary) stream."

	(anInteger < 0 or: [ anInteger >= 16r1000000 ])
		ifTrue: [self error: 'outside unsigned 24-bit integer range'].

	self nextPut: (anInteger byteAt: 3).
	self nextPut: (anInteger byteAt: 2).
	self nextPut: (anInteger byteAt: 1)
]

{ #category : #'accessing - data' }
PositionableStream >> uint32 [
	"Answer the next unsigned, 32-bit integer from this (binary) stream."

	| n |
	n := self next.
	n := (n bitShift: 8) + self next.
	n := (n bitShift: 8) + self next.
	n := (n bitShift: 8) + self next.
	^ n
]

{ #category : #'accessing - data' }
PositionableStream >> uint32: anInteger [
	"Store the given unsigned, 32-bit integer on this (binary) stream."

	(anInteger < 0 or: [ anInteger >= 16r100000000 ])
		ifTrue: [self error: 'outside unsigned 32-bit integer range'].

	self nextPut: (anInteger byteAt: 4).
	self nextPut: (anInteger byteAt: 3).
	self nextPut: (anInteger byteAt: 2).
	self nextPut: (anInteger byteAt: 1)
]

{ #category : #positioning }
PositionableStream >> untilEnd: aBlock displayingProgress: aString [
	aString
		displayProgressFrom: 0 to: self size
		during:
			[:bar |
			[self atEnd] whileFalse:
				[bar current: self position.
				aBlock value]]
]

{ #category : #positioning }
PositionableStream >> untilEndWithFork: aBlock displayingProgress: aString [
	| sem done result |
	sem := Semaphore new.
	done := false.
	[
	result := [ aBlock value ]
		ensure: [
			done := true.
			sem signal ] ] fork.
	self
		untilEnd: [
			done
				ifTrue: [ ^ result ].
			(Delay forSeconds: 0.2) wait ]
		displayingProgress: aString.
	sem wait.
	^ result
]

{ #category : #accessing }
PositionableStream >> upTo: anObject [
	"Answer a subcollection from the current access position to the
	occurrence (if any, but not inclusive) of anObject in the receiver. If
	anObject is not in the collection, answer the entire rest of the receiver."
	| newStream element |
	newStream := (collection species new: 100) writeStream.
	[self atEnd or: [(element := self next) = anObject]]
		whileFalse: [newStream nextPut: element].
	^newStream contents
]

{ #category : #accessing }
PositionableStream >> upToAll: aCollection [
	"Answer a subcollection from the current access position to the occurrence (if any, but not inclusive) of aCollection. If aCollection is not in the stream, answer the entire rest of the stream."

	| startPos endMatch result |
	startPos := self position.
	^ (self match: aCollection)
		ifTrue: [ endMatch := self position.
			self position: startPos.
			result := self upToPosition: endMatch - aCollection size.
			self position: endMatch.
			result ]
		ifFalse: [ self position: startPos.
			self upToEnd ]
]

{ #category : #accessing }
PositionableStream >> upToAny: aCollection [
	"Answer a subcollection from the current access position to the
	occurrence (if any, but not inclusive) of any objects in the given collection in the receiver. If
	any of these is not in the collection, answer the entire rest of the receiver."

	| newStream element |
	newStream := (collection species new: 100) writeStream.
	[self atEnd or: [aCollection includes: (element := self next)]]
		whileFalse: [newStream nextPut: element].
	^newStream contents
]

{ #category : #accessing }
PositionableStream >> upToAnyOf: aCollection [
	"Answer a subcollection from the current access position to the
	occurrence (if any, but not inclusive) of any object in the collection. If
	no matching object is found, answer the entire rest of the receiver."
	^self upToAnyOf: aCollection do: [:matchingObject | ]
]

{ #category : #accessing }
PositionableStream >> upToAnyOf: subcollection do: aBlock [
	"Answer a subcollection from the current access position to the occurrence (if any, but not inclusive) of any object in the collection.
	Evaluate aBlock with this occurrence as argument.
	If no matching object is found, don't evaluate aBlock and answer the entire rest of the receiver."

	^self collectionSpecies new: 1000 streamContents: [ :stream |
		| ch |
		[ self atEnd or: [ (subcollection includes: (ch := self next)) and: [aBlock value: ch. true] ] ]
			whileFalse: [ stream nextPut: ch ] ]
]

{ #category : #accessing }
PositionableStream >> upToEnd [
	"Answer a subcollection from the current access position through the last element of the receiver."

	| newStream |
	newStream := (collection species new: 100) writeStream.
	[self atEnd] whileFalse: [ newStream nextPut: self next ].
	^ newStream contents
]

{ #category : #accessing }
PositionableStream >> upToPosition: anInteger [
	"Answer a subcollection containing items starting from the current position and ending including the given position. Usefully different to #next: in that in the case of MultiByteFileStream, and perhaps others, positions measure in terms of encoded items, while #next: convention is to name a number of items, independent of their encoding in the underlying buffer."
	^ self next: anInteger - position
]
