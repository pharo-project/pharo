"
I am ZnLineReader and I help reading CRLF terminated lines from a stream.
I can work on ascii and binary streams doing conversions as necessary.
I limit lines to a maximum length.
I work with a reusable buffer.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnLineReader,
	#superclass : #Object,
	#instVars : [
		'stream',
		'buffer',
		'position',
		'limit'
	],
	#category : #'Zinc-HTTP-Streaming'
}

{ #category : #'instance creation' }
ZnLineReader class >> on: stream [
	^ self new
		on: stream;
		yourself
]

{ #category : #private }
ZnLineReader >> growBuffer [
	| newBuffer |
	buffer size >= limit 
		ifTrue: [
			(ZnLineTooLong limit: limit) signal ].
	newBuffer := String new: buffer size * 2.
	newBuffer replaceFrom: 1 to: buffer size with: buffer.
	buffer := newBuffer 
]

{ #category : #accessing }
ZnLineReader >> limit: size [
	limit := size
]

{ #category : #accessing }
ZnLineReader >> nextLine [
	self reset.
	[ self processNext isNil ] whileFalse.
	^ buffer copyFrom: 1 to: position 
]

{ #category : #'initialize-release' }
ZnLineReader >> on: readStream [
	stream := readStream.
	buffer := String new: 64.
	self 
		limit: ZnConstants maximumLineLength; 
		reset
]

{ #category : #private }
ZnLineReader >> processNext [
	| item |
	stream atEnd ifTrue: [ ^ nil ].
	(item := stream next) ifNil: [ ^ nil ].
	stream isBinary ifTrue: [ item := item asCharacter ].
	item == Character lf
		ifTrue: [
			(position > 0 and: [ (buffer at: position) == Character cr ])
				ifTrue: [
					position := position - 1.
					^ nil ] ].
	self store: item.
	^ item
			
	
]

{ #category : #initialization }
ZnLineReader >> reset [
	position := 0
]

{ #category : #private }
ZnLineReader >> store: item [
	position >= buffer size ifTrue: [ self growBuffer ].
	position := position + 1.
	buffer at: position put: item		
	
]
