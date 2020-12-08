"
This class absorb the difference of internal and external representation of the file path.  The idea is to keep the internal one as much as possible, and only when it goes to a primitive, the encoded file path, i.e. the native platform representation is passsed to the primitive.

	The converter used is obtained by ""LanguageEnvironment defaultFileNameConverter"".

"
Class {
	#name : #FilePathEncoder,
	#superclass : #Object,
	#category : #'Files-Directories'
}

{ #category : #encoding }
FilePathEncoder class >> decode: aString [
	^ aString utf8Decoded
]

{ #category : #encoding }
FilePathEncoder class >> encode: pathString [
	^ pathString utf8Encoded
]
