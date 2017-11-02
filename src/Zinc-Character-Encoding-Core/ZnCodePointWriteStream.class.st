"
I am ZnCodePointWriteStream.
I wrap another binary WriteStream and use a ZnCharacerEncoder to allow Integer code points to be written.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnCodePointWriteStream,
	#superclass : #ZnEncodedWriteStream,
	#category : 'Zinc-Character-Encoding-Core'
}

{ #category : #accessing }
ZnCodePointWriteStream >> nextPut: object [
	self encoder 
		nextPutCodePoint: object 
		toStream: stream.
	^ object
]
