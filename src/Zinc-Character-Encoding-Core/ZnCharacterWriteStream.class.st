"
I am ZnCharacterWriteStream.
I wrap another binary WriteStream and use a ZnCharacerEncoder to allow Characters to be written.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnCharacterWriteStream,
	#superclass : #ZnEncodedWriteStream,
	#category : #Zinc-Character-Encoding-Core
}

{ #category : #accessing }
ZnCharacterWriteStream >> cr [
	self nextPut: Character cr
]

{ #category : #accessing }
ZnCharacterWriteStream >> crlf [
	self cr; lf
]

{ #category : #accessing }
ZnCharacterWriteStream >> lf [
	self nextPut: Character lf
]

{ #category : #accessing }
ZnCharacterWriteStream >> next: count putAll: collection startingAt: offset [
	"Write count characters from collection starting at offset."
	
	self encoder 
		next: count 
		putAll: collection 
		startingAt: offset 
		toStream: stream 
]

{ #category : #accessing }
ZnCharacterWriteStream >> nextPut: object [
	self encoder 
		nextPut: object 
		toStream: stream.
	^ object
]

{ #category : #accessing }
ZnCharacterWriteStream >> print: object [
	object printOn: self
]

{ #category : #accessing }
ZnCharacterWriteStream >> space [
	self nextPut: Character space
]

{ #category : #accessing }
ZnCharacterWriteStream >> tab [
	self nextPut: Character tab
]
