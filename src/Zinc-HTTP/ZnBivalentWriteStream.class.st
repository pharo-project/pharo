"
I am ZnBivalentWriteStream, wrapping another write stream.

I allow both Characters and Strings as well as Integers (0 to: 255) and ByteArrays to be writtten to me.

The write stream that I wrap can be both binary as well as character based.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnBivalentWriteStream,
	#superclass : #Stream,
	#instVars : [
		'stream',
		'binary'
	],
	#category : #Zinc-HTTP-Streaming
}

{ #category : #'instance creation' }
ZnBivalentWriteStream class >> on: writeStream [
	"Wrap writeStream as a ZnBivalentWriteStream, unless it is already one"
	
	(writeStream isKindOf: self) ifTrue: [ ^ writeStream ].
	
	^ self basicNew
		on: writeStream;
		yourself
]

{ #category : #initialize-release }
ZnBivalentWriteStream >> close [
	stream close
]

{ #category : #accessing }
ZnBivalentWriteStream >> flush [
	stream flush
]

{ #category : #private }
ZnBivalentWriteStream >> isBinary [
	binary isNil
		ifTrue: [ [ binary := stream isBinary ] on: MessageNotUnderstood do: [ :err | binary := false ] ].
	^ binary
]

{ #category : #accessing }
ZnBivalentWriteStream >> next: count putAll: collection startingAt: offset [
	| buffer |
	self isBinary
		ifTrue: [ 
			(collection isKindOf: ByteArray)
				ifTrue: [ stream next: count putAll: collection startingAt: offset ]
				ifFalse: [ 
					buffer := ByteArray new: count.
					buffer replaceFrom: 1 to: count with: collection startingAt: offset.
					stream nextPutAll: buffer ] ]
		ifFalse: [ 
			(collection isKindOf: ByteArray)
				ifTrue: [ 
					buffer := String new: count.
					buffer replaceFrom: 1 to: count with: collection startingAt: offset.
					stream nextPutAll: buffer ]
				ifFalse: [ stream next: count putAll: collection startingAt: offset ] ]
]

{ #category : #accessing }
ZnBivalentWriteStream >> nextPut: object [
	self isBinary
		ifTrue: [ stream nextPut: object asInteger ]
		ifFalse: [ stream nextPut: object asCharacter ]
]

{ #category : #accessing }
ZnBivalentWriteStream >> nextPutAll: collection [
	self isBinary
		ifTrue: [ stream nextPutAll: collection asByteArray ]
		ifFalse: [ stream nextPutAll: collection asString ]
]

{ #category : #initialize-release }
ZnBivalentWriteStream >> on: writeStream [
	stream := writeStream
]

{ #category : #printing }
ZnBivalentWriteStream >> printOn: aStream [
	aStream 
		nextPutAll: 'a '; 
		nextPutAll: self class name
]

{ #category : #accessing }
ZnBivalentWriteStream >> space [
	self nextPut: Character space
]
