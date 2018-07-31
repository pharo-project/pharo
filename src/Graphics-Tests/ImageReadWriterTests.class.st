"
Functional tests for image IO to binary streams and files for the major formats (JPEG,PNG,GIF,BMP)
"
Class {
	#name : #ImageReadWriterTests,
	#superclass : #TestCase,
	#category : #'Graphics-Tests-Files'
}

{ #category : #accessing }
ImageReadWriterTests >> pharoLogoForm [
	^ (PNGReadWriter on: (ZnBase64Encoder new decode: PolymorphSystemSettings pharoLogoContents) readStream) nextImage
]

{ #category : #accessing }
ImageReadWriterTests >> pharoLogoFormDepth8 [
	^ self pharoLogoFormNonTransparent asFormOfDepth: 8
]

{ #category : #accessing }
ImageReadWriterTests >> pharoLogoFormNonTransparent [
	^ self pharoLogoForm 
		mapColor: Color transparent to: Color white;
		yourself
]

{ #category : #tests }
ImageReadWriterTests >> testBmpWriteReadInMemory [
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

{ #category : #tests }
ImageReadWriterTests >> testBmpWriteReadUsingFiles [
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

{ #category : #tests }
ImageReadWriterTests >> testGifWriteReadInMemory [
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

{ #category : #tests }
ImageReadWriterTests >> testGifWriteReadUsingFiles [
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

{ #category : #tests }
ImageReadWriterTests >> testJpegWriteReadInMemory [
	| form bytes result |
	form := self pharoLogoFormNonTransparent.
	bytes := ByteArray streamContents: [ :out | PluginBasedJPEGReadWriter putForm: form onStream: out ].
	"Without format detection"
	result := (JPEGReadWriter on: bytes readStream) nextImage.
	self assert: form bits size equals: result bits size.
	"With format detection"
	result := ImageReadWriter formFromStream: bytes readStream.
	self assert: form bits size equals: result bits size
]

{ #category : #tests }
ImageReadWriterTests >> testJpegWriteReadUsingFiles [
	| form file result |
	form := self pharoLogoFormNonTransparent.
	file := (self class name asString , '-pharo-logo.jpeg') asFileReference.
	file ensureDelete.
	(PluginBasedJPEGReadWriter on: file binaryWriteStream) nextPutImage: form. "gets closed ?!"
	"Without format detection"
	result := file binaryReadStreamDo: [ :in | (JPEGReadWriter on: in) nextImage ].
	self assert: form bits size equals: result bits size.
	"With format detection"
	result := file binaryReadStreamDo: [ :in | ImageReadWriter formFromStream: in ].
	self assert: form bits size equals: result bits size.
	file delete
]

{ #category : #tests }
ImageReadWriterTests >> testPluginBasedJpegWriteReadInMemory [
	| form bytes result |
	form := self pharoLogoFormNonTransparent.
	bytes := ByteArray streamContents: [ :out | PluginBasedJPEGReadWriter putForm: form onStream: out ].
	"Without format detection"
	result := (PluginBasedJPEGReadWriter on: bytes readStream) nextImage.
	self assert: form bits size equals: result bits size.
	"With format detection"
	result := ImageReadWriter formFromStream: bytes readStream.
	self assert: form bits size equals: result bits size
]

{ #category : #tests }
ImageReadWriterTests >> testPluginBasedJpegWriteReadUsingFiles [
	| form file result |
	form := self pharoLogoFormNonTransparent.
	file := (self class name asString , '-pharo-logo.jpeg') asFileReference.
	file ensureDelete.
	(PluginBasedJPEGReadWriter on: file binaryWriteStream) nextPutImage: form. "gets closed ?!"
	"Without format detection"
	result := file binaryReadStreamDo: [ :in | (PluginBasedJPEGReadWriter on: in) nextImage ].
	self assert: form bits size equals: result bits size.
	"With format detection"
	result := file binaryReadStreamDo: [ :in | ImageReadWriter formFromStream: in ].
	self assert: form bits size equals: result bits size.
	file delete
]

{ #category : #tests }
ImageReadWriterTests >> testPngWriteReadInMemory [
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

{ #category : #tests }
ImageReadWriterTests >> testPngWriteReadUsingFiles [
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
