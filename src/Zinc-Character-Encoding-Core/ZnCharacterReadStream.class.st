"
I am ZnCharacterReadStream.
I wrap another binary ReadStream and use a ZnCharacerEncoder to allow Characters to be read.

I am not positionable, but I do allow a one character peek using a one character internal buffer.

Part of Zinc HTTP Components.

"
Class {
	#name : #ZnCharacterReadStream,
	#superclass : #ZnEncodedReadStream,
	#category : 'Zinc-Character-Encoding-Core'
}

{ #category : #accessing }
ZnCharacterReadStream >> collectionSpecies [
	^ String
]

{ #category : #private }
ZnCharacterReadStream >> nextElement [
	^ self encoder nextFromStream: stream
]

{ #category : #accessing }
ZnCharacterReadStream >> nextLine [
	"Read a CR, LF or CRLF terminated line, returning the contents of the line without the EOL. Return nil when the receiver is #atEnd."
	
	self atEnd ifTrue: [ ^ nil ].
	^ self collectionSpecies streamContents: [ :out | | eol char |
		eol := false.
		[ eol ] whileFalse: [ 
			char := self next.
			(char isNil or: [ char = Character lf ])
				ifTrue: [ eol := true ]
				ifFalse: [ 
					char = Character cr 
						ifTrue: [ eol := true. self peekFor: Character lf ]
						ifFalse: [ out nextPut: char ] ] ] ]
]

{ #category : #accessing }
ZnCharacterReadStream >> readInto: collection startingAt: offset count: requestedCount [ 
	"Read count elements and place them in collection starting at offset.
	Return the number of elements actually read."
	
	^ peeked 
		ifNil: [ | readCount |
			[ readCount := self encoder 
					readInto: collection 
					startingAt: offset 
					count: requestedCount 
					fromStream: stream ]
				on: ZnByteStringBecameWideString 
				do: [ :byteStringBecameWideString | 
					byteStringBecameWideString becomeForward; resume ].
			readCount ]
		ifNotNil: [ 
			collection at: offset put: peeked.
			peeked := nil.
			(self 
				readInto: collection 
				startingAt: offset + 1
				count: requestedCount - 1) + 1 ]
]
