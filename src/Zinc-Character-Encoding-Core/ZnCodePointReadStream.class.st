"
I am ZnCodePointReadStream.
I wrap another binary ReadStream and use a ZnCharacerEncoder to allow Integer code points to be read.

I am not positionable, but I do allow a one code point peek using a one code point internal buffer.

Part of Zinc HTTP Components.

"
Class {
	#name : #ZnCodePointReadStream,
	#superclass : #ZnEncodedReadStream,
	#category : #'Zinc-Character-Encoding-Core'
}

{ #category : #accessing }
ZnCodePointReadStream >> collectionSpecies [
	^ Array
]

{ #category : #private }
ZnCodePointReadStream >> nextElement [
	^ self encoder nextCodePointFromStream: stream
]
