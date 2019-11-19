"
I am STONCStyleCommentsSkipStream, a character ReadStream wrapping another character ReadStream. I skip C style comments, much like a classic C preprocessor.

C, C++, Java, JavaScript style comments are either the multiline

	/* a comment */
	
or the single line, up to end of line

	// a comment 
	
I deal with any end of line convention. Multiline comments cannot be nested. 

You create me #on: another character ReadStream. 

Here is an example:

	(STONCStyleCommentsSkipStream on: 'abc/*comment*/def' readStream) upToEnd.
	
Comments inside single and double quote delimited strings are ignored. Backslash escapes for single and double quotes inside strings are honored.

"
Class {
	#name : #STONCStyleCommentsSkipStream,
	#superclass : #Object,
	#instVars : [
		'stream',
		'peekedCharacter',
		'delimiter',
		'escape'
	],
	#category : #'STON-Core-Reader'
}

{ #category : #'instance creation' }
STONCStyleCommentsSkipStream class >> on: readStream [
	^ self new
		on: readStream;
		yourself
]

{ #category : #testing }
STONCStyleCommentsSkipStream >> atEnd [
	^ self peek isNil
]

{ #category : #'initialize-release' }
STONCStyleCommentsSkipStream >> close [
	stream close
]

{ #category : #accessing }
STONCStyleCommentsSkipStream >> collectionSpecies [
	^ String
]

{ #category : #private }
STONCStyleCommentsSkipStream >> consumeComment [
	stream peek = $/ ifTrue: [ self consumeToEOL ].
	stream peek = $* ifTrue: [ self consumeToCommentEnd ]
]

{ #category : #private }
STONCStyleCommentsSkipStream >> consumeToCommentEnd [
	[ stream atEnd or: [ stream next = $* and: [ stream peekFor: $/ ] ] ] whileFalse
]

{ #category : #private }
STONCStyleCommentsSkipStream >> consumeToEOL [
	| eol char |
	eol := false.
	[ eol ] whileFalse: [ 
		char := stream next.
		(char isNil or: [ char = Character lf ])
			ifTrue: [ eol := true ]
			ifFalse: [ 
				char = Character cr
					ifTrue: [ 
						eol := true.
						stream peekFor: Character lf ] ] ]
]

{ #category : #private }
STONCStyleCommentsSkipStream >> escape [
	"Return true when we previously read a backslash escape inside a string, 
	so that the next string delimiter should be returned as is"
	
	^ escape = true
]

{ #category : #private }
STONCStyleCommentsSkipStream >> handleStringDelimiter: char [
	self escape
		ifTrue: [ escape := false ]
		ifFalse: [ 
			self insideString
				ifTrue: [ 
					char = delimiter
						ifTrue: [ delimiter := nil ] ]
				ifFalse: [ delimiter := char ] ].
	^ char
]

{ #category : #private }
STONCStyleCommentsSkipStream >> insideString [
	"Return true when we are currently inside a string where comments should be ignored."
	
	^ (delimiter = $') | (delimiter = $")
]

{ #category : #testing }
STONCStyleCommentsSkipStream >> isBinary [
	^ false
]

{ #category : #accessing }
STONCStyleCommentsSkipStream >> next [
	^ peekedCharacter
		ifNil: [ 
			stream atEnd
				ifFalse: [ self nextNonCommentChar ] ]
		ifNotNil: [ | character |
			character := peekedCharacter.
			peekedCharacter := nil.
			character ]
]

{ #category : #accessing }
STONCStyleCommentsSkipStream >> next: requestedCount [ 
	"Read requestedCount elements into new collection and return it,
	 it could be that less elements were available"

	^ self 
		next: requestedCount 
		into: (self collectionSpecies new: requestedCount)
]

{ #category : #accessing }
STONCStyleCommentsSkipStream >> next: requestedCount into: collection [
	"Read requestedCount elements into collection,
	returning a copy if less elements are available"
	
	^ self
		next: requestedCount
		into: collection
		startingAt: 1
]

{ #category : #accessing }
STONCStyleCommentsSkipStream >> next: requestedCount into: collection startingAt: offset [
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

{ #category : #accessing }
STONCStyleCommentsSkipStream >> nextLine [
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
						ifFalse: [  out nextPut: char ] ] ] ]
]

{ #category : #private }
STONCStyleCommentsSkipStream >> nextNonCommentChar [
	| char |
	char := stream next.
	(self insideString and: [ char = $\ ])
		ifTrue: [ 
			escape := true. 
			^ char ].
	(char = $') | (char = $")
		ifTrue: [ 
			^ self handleStringDelimiter: char ].
	escape := false. 
	^ (char = $/ and: [ self insideString not and: [ (stream peek = $/) | (stream peek = $*) ] ])
		ifTrue: [ 
			self consumeComment.
			stream next ]
		ifFalse: [ char ]

]

{ #category : #'initialize-release' }
STONCStyleCommentsSkipStream >> on: readStream [
	stream := readStream
]

{ #category : #accessing }
STONCStyleCommentsSkipStream >> peek [
	^ peekedCharacter
		ifNil: [
			stream atEnd
				ifFalse: [ 
					peekedCharacter := self nextNonCommentChar ] ]

]

{ #category : #accessing }
STONCStyleCommentsSkipStream >> peekFor: object [
	^ self peek = object
		ifTrue: [ 
			self next.
			true ]
		ifFalse: [ false ]
]

{ #category : #accessing }
STONCStyleCommentsSkipStream >> position [
	^ stream position
]

{ #category : #accessing }
STONCStyleCommentsSkipStream >> readInto: collection startingAt: offset count: requestedCount [ 
	"Read count elements and place them in collection starting at offset.
	Return the number of elements actually read."
	
	^ peekedCharacter 
		ifNil: [
			0 to: requestedCount - 1 do: [ :count | | object |
				(object := self nextNonCommentChar) ifNil: [ ^ count ].  
				collection at: offset + count put: object ].
			^ requestedCount ]
		ifNotNil: [ 
			collection at: offset put: peekedCharacter.
			peekedCharacter := nil.
			(self 
				readInto: collection 
				startingAt: offset + 1
				count: requestedCount - 1) + 1 ]
]

{ #category : #accessing }
STONCStyleCommentsSkipStream >> skip: count [
	count timesRepeat: [ self next ]
]

{ #category : #accessing }
STONCStyleCommentsSkipStream >> upTo: anObject [ 	
	^ self collectionSpecies 
		streamContents: [ :out | | element |
			[ self atEnd or: [ (element := self next) = anObject ] ] whileFalse: [ 
				out nextPut: element ] ]
]

{ #category : #accessing }
STONCStyleCommentsSkipStream >> upToEnd [
	^ self collectionSpecies
		streamContents: [ :collectionStream | 
			[ self atEnd ] whileFalse: [ collectionStream nextPut: self next ] ]
]

{ #category : #accessing }
STONCStyleCommentsSkipStream >> wrappedStream [
	^ stream
]
