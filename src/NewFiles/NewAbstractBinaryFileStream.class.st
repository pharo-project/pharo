Class {
	#name : 'NewAbstractBinaryFileStream',
	#superclass : 'Stream',
	#instVars : [
		'file',
		'forWrite'
	],
	#pools : [
		'NewFileConstants'
	],
	#category : 'NewFiles-Streams',
	#package : 'NewFiles',
	#tag : 'Streams'
}

{ #category : 'construction' }
NewAbstractBinaryFileStream class >> forFile: aFile forWrite: aBoolean [
	^ self basicNew
		initializeWithFile: aFile forWrite: aBoolean;
		yourself
]

{ #category : 'testing' }
NewAbstractBinaryFileStream >> atEnd [
	^ file atEnd
]

{ #category : 'open/close' }
NewAbstractBinaryFileStream >> close [
	file ifNotNil: [ file close ].
	file := nil
]

{ #category : 'accessing' }
NewAbstractBinaryFileStream >> contents [
	^ file contents
]

{ #category : 'character writing' }
NewAbstractBinaryFileStream >> cr [
	self nextPut: Character cr asInteger.
]

{ #category : 'character writing' }
NewAbstractBinaryFileStream >> crlf [
	self nextPutAll: String crlf
]

{ #category : 'accessing' }
NewAbstractBinaryFileStream >> file [

	^ file
]

{ #category : 'accessing' }
NewAbstractBinaryFileStream >> file: anObject [

	file := anObject
]

{ #category : 'flushing' }
NewAbstractBinaryFileStream >> flush [
	self flag: 'Todo implement flushing'.
	
]

{ #category : 'accessing' }
NewAbstractBinaryFileStream >> forWrite [

	^ forWrite
]

{ #category : 'accessing' }
NewAbstractBinaryFileStream >> forWrite: anObject [

	forWrite := anObject
]

{ #category : 'initialization' }
NewAbstractBinaryFileStream >> initializeWithFile: aFile forWrite: aBoolean [
	file := aFile.
	forWrite := aBoolean.
]

{ #category : 'testing' }
NewAbstractBinaryFileStream >> isBinary [
	^ true
]

{ #category : 'character writing' }
NewAbstractBinaryFileStream >> lf [
	self nextPut: Character lf asInteger.
]

{ #category : 'accessing' }
NewAbstractBinaryFileStream >> next [
	"Answer the next byte from this file, or nil if at the end of the file."

	^ (self next: 1) ifEmpty: [ nil ] ifNotEmpty: [ :col | col first ]
]

{ #category : 'accessing' }
NewAbstractBinaryFileStream >> next: n [
	"Return a string with the next n characters of the filestream in it."

	^ self next: n into: (ByteArray new: n)
]

{ #category : 'accessing' }
NewAbstractBinaryFileStream >> next: n into: aBuffer [
	"Return a string with the next n characters of the filestream in it."
	| readBuffer read |
	readBuffer := aBuffer.
	read := file readInto: aBuffer offset: 0 withSize: n.
	^read = n
		ifTrue: [ readBuffer ]
		ifFalse: [ readBuffer copyFrom: 1 to: read ]
]

{ #category : 'character writing' }
NewAbstractBinaryFileStream >> next: amount putAll: aByteArray [

	^ self next: amount putAll: aByteArray startingAt: 1
]

{ #category : 'character writing' }
NewAbstractBinaryFileStream >> next: amount putAll: aByteArray startingAt: startingIndex [

	forWrite
		ifFalse: [ ^ self error: 'Cannot write a read-only file' ].
	file writeFrom: aByteArray offset: startingIndex - 1 withSize: amount.
	^ aByteArray
]

{ #category : 'character writing' }
NewAbstractBinaryFileStream >> nextPut: anInteger [

	^ self nextPutAll: (ByteArray with: anInteger asInteger)
]

{ #category : 'character writing' }
NewAbstractBinaryFileStream >> nextPutAll: aByteArray [
	self next: aByteArray basicSize putAll: aByteArray
]

{ #category : 'accessing' }
NewAbstractBinaryFileStream >> position [
	^ file position
]

{ #category : 'accessing' }
NewAbstractBinaryFileStream >> position: newPosition [
	^ file position: newPosition
]

{ #category : 'accessing' }
NewAbstractBinaryFileStream >> size [

	^ file size
]

{ #category : 'accessing' }
NewAbstractBinaryFileStream >> skip: n [
	"Set the character position to n characters from the current position.
	Error if not enough characters left in the file.
	By default we read n characters and we avoid reading the output"
	self next: n
]

{ #category : 'flushing' }
NewAbstractBinaryFileStream >> sync [
	self flag: 'Todo implement sync'.
	
]

{ #category : 'accessing' }
NewAbstractBinaryFileStream >> writeFrom: writeBuffer startingAt: aNumber count: length [
	^ file writeFrom: writeBuffer offset: aNumber - 1 withSize: length
]
