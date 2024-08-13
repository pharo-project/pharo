"
Functional tests for image IO to binary streams and files for the major formats (JPEG,PNG,GIF,BMP)
"
Class {
	#name : 'ImageReadWriterTest',
	#superclass : 'TestCase',
	#category : 'Graphics-Tests-Files',
	#package : 'Graphics-Tests',
	#tag : 'Files'
}

{ #category : 'accessing' }
ImageReadWriterTest >> pharoLogoForm [
	^ (PNGReadWriter on: (ZnBase64Encoder new decode: PolymorphSystemSettings pharoLogoContents) readStream) nextImage
]

{ #category : 'accessing' }
ImageReadWriterTest >> pharoLogoFormDepth8 [
	^ self pharoLogoFormNonTransparent asFormOfDepth: 8
]

{ #category : 'accessing' }
ImageReadWriterTest >> pharoLogoFormNonTransparent [
	^ self pharoLogoForm
		mapColor: Color transparent to: Color white;
		yourself
]

{ #category : 'tests' }
ImageReadWriterTest >> testBmpWriteReadInMemory [
	| form bytes result |
	form := self pharoLogoFormNonTransparent.
	bytes := ByteArray streamContents: [ :out | BMPReadWriter putForm: form onStream: out ].
	"Without format detection"
	result := (BMPReadWriter on: bytes readStream) nextImage.
	self assert: form bits equals: result bits.
	"With format detection"
	result := ImageReadWriter formFromStream: bytes readStream.
	self assert: form bits equals: result bits
]

{ #category : 'tests' }
ImageReadWriterTest >> testBmpWriteReadUsingFiles [
	| form file result |
	form := self pharoLogoFormNonTransparent.
	file := (self class name asString , '-pharo-logo.bmp') asFileReference.
	file ensureDelete.
	file binaryWriteStreamDo: [ :out | (BMPReadWriter on: out) nextPutImage: form ].
	"Without format detection"
	result := file binaryReadStreamDo: [ :in | (BMPReadWriter on: in) nextImage ].
	self assert: form bits equals: result bits.
	"With format detection"
	result := file binaryReadStreamDo: [ :in | ImageReadWriter formFromStream: in ].
	"result := ImageReadWriter formFromFileNamed: file fullPath."
	self assert: form bits equals: result bits.
	file delete
]

{ #category : 'tests' }
ImageReadWriterTest >> testCanGetFormOutOfBase64EncodedStringImage [
	| encodedImage form expextedFrom |
	encodedImage := PolymorphSystemSettings pharoLogoContents.
	expextedFrom := self pharoLogoForm.

	form := Form fromBase64String: encodedImage.

	self
		assert: form width
		equals: expextedFrom width.
	self
		assert: form height
		equals: expextedFrom height.
	self
		assert: form depth
		equals: expextedFrom depth.
	self
		assert: form bits
		equals: self pharoLogoForm bits
]

{ #category : 'tests' }
ImageReadWriterTest >> testGifWriteReadInMemory [
	| form bytes result |
	form := self pharoLogoFormDepth8.
	bytes := ByteArray streamContents: [ :out | GIFReadWriter putForm: form onStream: out ].
	"Without format detection"
	result := (GIFReadWriter on: bytes readStream) nextImage.
	self assert: form bits equals: result bits.
	"With format detection"
	result := ImageReadWriter formFromStream: bytes readStream.
	self assert: form bits equals: result bits
]

{ #category : 'tests' }
ImageReadWriterTest >> testGifWriteReadUsingFiles [
	| form file result |
	form := self pharoLogoFormDepth8.
	file := (self class name asString , '-pharo-logo.gif') asFileReference.
	file ensureDelete.
	file binaryWriteStreamDo: [ :out | (GIFReadWriter on: out) nextPutImage: form ].
	"Without format detection"
	result := file binaryReadStreamDo: [ :in | (GIFReadWriter on: in) nextImage ].
	self assert: form bits equals: result bits.
	"With format detection"
	result := file binaryReadStreamDo: [ :in | ImageReadWriter formFromStream: in ].
	self assert: form bits equals: result bits.
	file delete
]

{ #category : 'tests' }
ImageReadWriterTest >> testHasMagicNumber [
	"Tests now account for the new Streams, for which we cannot
	 assume we will be able to know the whole size"
	| exampleNumber writerStream failStream failTooShortStream reader |
	exampleNumber := 'GIF89a'.
	writerStream := ZnPositionableReadStream on: (ReadStream on: 'GIF89a this is some data').
	failStream := ZnPositionableReadStream on: (ReadStream on: 'This is incorrect').
	failTooShortStream := ZnPositionableReadStream on: (ReadStream on: 'GIF').

	"This one has the magic number at the current position"
	reader := ImageReadWriter on: writerStream.
	self assert: (reader hasMagicNumber: exampleNumber).

	"Does not have the magic number at the current position"
	reader := ImageReadWriter on: failStream.
	self assert: (reader hasMagicNumber: exampleNumber) equals: false.

	"The source stream is shorter than the magic number. Shouldn't error!"
	reader := ImageReadWriter on: failTooShortStream.
	self assert: (reader hasMagicNumber: exampleNumber) equals: false
]

{ #category : 'tests' }
ImageReadWriterTest >> testJpegWriteReadInMemory [
	| form bytes result |
	form := self pharoLogoFormNonTransparent.
	bytes := ByteArray streamContents: [ :out | JPEGReadWriter putForm: form onStream: out ].
	"Without format detection"
	result := (JPEGReadWriter on: bytes readStream) nextImage.
	self assert: form bits size equals: result bits size.
	"With format detection"
	result := ImageReadWriter formFromStream: bytes readStream.
	self assert: form bits size equals: result bits size
]

{ #category : 'tests' }
ImageReadWriterTest >> testJpegWriteReadUsingFiles [
	| form file result |
	form := self pharoLogoFormNonTransparent.
	file := (self class name asString , '-pharo-logo.jpeg') asFileReference.
	file ensureDelete.
	(JPEGReadWriter on: file binaryWriteStream) nextPutImage: form. "gets closed ?!"
	"Without format detection"
	result := file binaryReadStreamDo: [ :in | (JPEGReadWriter on: in) nextImage ].
	self assert: form bits size equals: result bits size.
	"With format detection"
	result := file binaryReadStreamDo: [ :in | ImageReadWriter formFromStream: in ].
	self assert: form bits size equals: result bits size.
	file delete
]

{ #category : 'tests' }
ImageReadWriterTest >> testPluginBasedJpegWriteReadInMemory [
	| form bytes result |
	form := self pharoLogoFormNonTransparent.
	bytes := ByteArray streamContents: [ :out | JPEGReadWriter putForm: form onStream: out ].
	"Without format detection"
	result := (JPEGReadWriter on: bytes readStream) nextImage.
	self assert: form bits size equals: result bits size.
	"With format detection"
	result := ImageReadWriter formFromStream: bytes readStream.
	self assert: form bits size equals: result bits size
]

{ #category : 'tests' }
ImageReadWriterTest >> testPluginBasedJpegWriteReadUsingFiles [
	| form file result |
	form := self pharoLogoFormNonTransparent.
	file := (self class name asString , '-pharo-logo.jpeg') asFileReference.
	file ensureDelete.
	(JPEGReadWriter on: file binaryWriteStream) nextPutImage: form. "gets closed ?!"
	"Without format detection"
	result := file binaryReadStreamDo: [ :in | (JPEGReadWriter on: in) nextImage ].
	self assert: form bits size equals: result bits size.
	"With format detection"
	result := file binaryReadStreamDo: [ :in | ImageReadWriter formFromStream: in ].
	self assert: form bits size equals: result bits size.
	file delete
]

{ #category : 'tests' }
ImageReadWriterTest >> testPngWriteReadInMemory [
	| form bytes result |
	form := self pharoLogoForm.
	bytes := ByteArray streamContents: [ :out | PNGReadWriter putForm: form onStream: out ].
	"Without format detection"
	result := (PNGReadWriter on: bytes readStream) nextImage.
	self assert: form bits equals: result bits.
	"With format detection"
	result := ImageReadWriter formFromStream: bytes readStream.
	self assert: form bits equals: result bits
]

{ #category : 'tests' }
ImageReadWriterTest >> testPngWriteReadUsingFiles [
	| form file result |
	form := self pharoLogoForm.
	file := (self class name asString , '-pharo-logo.png') asFileReference.
	file ensureDelete.
	file binaryWriteStreamDo: [ :out | (PNGReadWriter on: out) nextPutImage: form ].
	"Without format detection"
	result := file binaryReadStreamDo: [ :in | (PNGReadWriter on: in) nextImage ].
	self assert: form bits equals: result bits.
	"With format detection"
	result := file binaryReadStreamDo: [ :in | ImageReadWriter formFromStream: in ].
	self assert: form bits equals: result bits.
	file delete
]
