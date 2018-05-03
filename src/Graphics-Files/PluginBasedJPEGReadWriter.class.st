"
I provide fast JPEG compression and decompression. I require the VM pluginJPEGReadWriter2Plugin, which is typically stored in same directory as the Squeak virtual machine.

JPEGReadWriter2Plugin is based on LIBJPEG library. This sentence applies to the plugin:
   ""This software is based in part on the work of the Independent JPEG Group"".

The LIBJPEG license allows it to be used free for any purpose so long as its origin and copyright are acknowledged. You can read more about LIBJPEG and get the complete source code at www.ijg.org.

"
Class {
	#name : #PluginBasedJPEGReadWriter,
	#superclass : #ImageReadWriter,
	#category : #'Graphics-Files'
}

{ #category : #'image reading/writing' }
PluginBasedJPEGReadWriter class >> primJPEGPluginIsPresent [
	<primitive: 'primJPEGPluginIsPresent' module: 'JPEGReadWriter2Plugin'>
	^false
]

{ #category : #'image reading/writing' }
PluginBasedJPEGReadWriter class >> putForm: aForm onFileNamed: fileName [ 

	self 
	putForm: aForm 
	quality: -1 
	progressiveJPEG: false
	onFileNamed: fileName
]

{ #category : #'image reading/writing' }
PluginBasedJPEGReadWriter class >> putForm: aForm quality: quality progressiveJPEG: progressiveFlag onFileNamed: fileName [ 
	"Store the given Form as a JPEG file of the given name, overwriting any existing file of that name. Quality goes from 0 (low) to 100 (high), where -1 means default. If progressiveFlag is true, encode as a progressive JPEG."
	| writer |
	fileName asFileReference ensureDelete.
	writer := self on: fileName asFileReference binaryWriteStream.
	writer 
		nextPutImage: aForm
		quality: quality
		progressiveJPEG: progressiveFlag.
	writer close
]

{ #category : #'image reading/writing' }
PluginBasedJPEGReadWriter class >> typicalFileExtensions [
	"Answer a collection of file extensions (lowercase) which files that I can read might commonly have"
	^#('jpg' 'jpeg')
]

{ #category : #'public access' }
PluginBasedJPEGReadWriter >> compress: aForm quality: quality [ 
	"Encode the given Form and answer the compressed ByteArray. Quality goes from 0 (low) to 100 (high), where -1 means default."
	| sourceForm jpegCompressStruct jpegErrorMgr2Struct buffer byteCount |
	aForm unhibernate.
	"odd width images of depth 16 give problems; avoid them."
	sourceForm := aForm depth = 32 | (aForm width even & (aForm depth = 16)) 
		ifTrue: [ aForm ]
		ifFalse: [ aForm asFormOfDepth: 32 ].
	jpegCompressStruct := ByteArray new: self primJPEGCompressStructSize.
	jpegErrorMgr2Struct := ByteArray new: self primJPEGErrorMgr2StructSize.
	buffer := ByteArray new: sourceForm width * sourceForm height + 1024.
	byteCount := self 
		primJPEGWriteImage: jpegCompressStruct
		onByteArray: buffer
		form: sourceForm
		quality: quality
		progressiveJPEG: false
		errorMgr: jpegErrorMgr2Struct.
	byteCount = 0 ifTrue: [ self error: 'buffer too small for compressed data' ].
	^ buffer 
		copyFrom: 1
		to: byteCount
]

{ #category : #'public access' }
PluginBasedJPEGReadWriter >> imageExtent: aByteArray [ 
	"Answer the extent of the compressed image encoded in the given ByteArray."
	| jpegDecompressStruct jpegErrorMgr2Struct w h |
	jpegDecompressStruct := ByteArray new: self primJPEGDecompressStructSize.
	jpegErrorMgr2Struct := ByteArray new: self primJPEGErrorMgr2StructSize.
	self 
		primJPEGReadHeader: jpegDecompressStruct
		fromByteArray: aByteArray
		errorMgr: jpegErrorMgr2Struct.
	w := self primImageWidth: jpegDecompressStruct.
	h := self primImageHeight: jpegDecompressStruct.
	^ w @ h
]

{ #category : #testing }
PluginBasedJPEGReadWriter >> isPluginPresent [
	^self primJPEGPluginIsPresent
]

{ #category : #'public access' }
PluginBasedJPEGReadWriter >> nextImage [
	"Decode and answer a Form from my stream."

	^ self nextImageSuggestedDepth: Display depth

]

{ #category : #'public access' }
PluginBasedJPEGReadWriter >> nextImageSuggestedDepth: depth [ 
	"Decode and answer a Form of the given depth from my stream. Close the stream if it is a file stream. Possible depths are 16-bit and 32-bit."
	| bytes width height form jpegDecompressStruct jpegErrorMgr2Struct depthToUse |
	bytes := stream upToEnd.
	stream close.
	jpegDecompressStruct := ByteArray new: self primJPEGDecompressStructSize.
	jpegErrorMgr2Struct := ByteArray new: self primJPEGErrorMgr2StructSize.
	self 
		primJPEGReadHeader: jpegDecompressStruct
		fromByteArray: bytes
		errorMgr: jpegErrorMgr2Struct.
	width := self primImageWidth: jpegDecompressStruct.
	height := self primImageHeight: jpegDecompressStruct.
	"Odd width images of depth 16 gave problems. Avoid them (or check carefully!)"
	depthToUse := depth = 32 | width odd 
		ifTrue: [ 32 ]
		ifFalse: [ 16 ].
	form := Form 
		extent: width @ height
		depth: depthToUse.
	(width = 0 or: [ height = 0 ]) ifTrue: [ ^ form ].
	self 
		primJPEGReadImage: jpegDecompressStruct
		fromByteArray: bytes
		onForm: form
		doDithering: true
		errorMgr: jpegErrorMgr2Struct.
	^ form
]

{ #category : #'public access' }
PluginBasedJPEGReadWriter >> nextPutImage: aForm [
	"Encode the given Form on my stream with default quality."

	^ self nextPutImage: aForm quality: -1 progressiveJPEG: false

]

{ #category : #'public access' }
PluginBasedJPEGReadWriter >> nextPutImage: aForm quality: quality progressiveJPEG: progressiveFlag [ 
	"Encode the given Form on my stream with the given settings. Quality goes from 0 (low) to 100 (high), where -1 means default. If progressiveFlag is true, encode as a progressive JPEG."
	| sourceForm jpegCompressStruct jpegErrorMgr2Struct buffer byteCount |
	aForm unhibernate.
	"odd width images of depth 16 give problems; avoid them."
	sourceForm := aForm depth = 32 | (aForm width even & (aForm depth = 16)) 
		ifTrue: [ aForm ]
		ifFalse: [ aForm asFormOfDepth: 32 ].
	jpegCompressStruct := ByteArray new: self primJPEGCompressStructSize.
	jpegErrorMgr2Struct := ByteArray new: self primJPEGErrorMgr2StructSize.
	buffer := ByteArray new: sourceForm width * sourceForm height + 1024.
	byteCount := self 
		primJPEGWriteImage: jpegCompressStruct
		onByteArray: buffer
		form: sourceForm
		quality: quality
		progressiveJPEG: progressiveFlag
		errorMgr: jpegErrorMgr2Struct.
	byteCount = 0 ifTrue: [ self error: 'buffer too small for compressed data' ].
	stream 
		next: byteCount
		putAll: buffer
		startingAt: 1.
	self close
]

{ #category : #primitives }
PluginBasedJPEGReadWriter >> primImageHeight: aJPEGCompressStruct [

	<primitive: 'primImageHeight' module: 'JPEGReadWriter2Plugin'>
	self primitiveFailed

]

{ #category : #primitives }
PluginBasedJPEGReadWriter >> primImageWidth: aJPEGCompressStruct [

	<primitive: 'primImageWidth' module: 'JPEGReadWriter2Plugin'>
	self primitiveFailed

]

{ #category : #primitives }
PluginBasedJPEGReadWriter >> primJPEGCompressStructSize [

	<primitive: 'primJPEGCompressStructSize' module: 'JPEGReadWriter2Plugin'>
	self primitiveFailed

]

{ #category : #primitives }
PluginBasedJPEGReadWriter >> primJPEGDecompressStructSize [

	<primitive: 'primJPEGDecompressStructSize' module: 'JPEGReadWriter2Plugin'>
	self primitiveFailed

]

{ #category : #primitives }
PluginBasedJPEGReadWriter >> primJPEGErrorMgr2StructSize [

	<primitive: 'primJPEGErrorMgr2StructSize' module: 'JPEGReadWriter2Plugin'>
	self primitiveFailed

]

{ #category : #primitives }
PluginBasedJPEGReadWriter >> primJPEGPluginIsPresent [
	<primitive: 'primJPEGPluginIsPresent' module: 'JPEGReadWriter2Plugin'>
	^false
]

{ #category : #primitives }
PluginBasedJPEGReadWriter >> primJPEGReadHeader: aJPEGDecompressStruct fromByteArray: source errorMgr: aJPEGErrorMgr2Struct [

	<primitive: 'primJPEGReadHeaderfromByteArrayerrorMgr' module: 'JPEGReadWriter2Plugin'>
	self primitiveFailed

]

{ #category : #primitives }
PluginBasedJPEGReadWriter >> primJPEGReadImage: aJPEGDecompressStruct fromByteArray: source onForm: form doDithering: ditherFlag errorMgr: aJPEGErrorMgr2Struct [

	<primitive: 'primJPEGReadImagefromByteArrayonFormdoDitheringerrorMgr' module: 'JPEGReadWriter2Plugin'>
	self primitiveFailed

]

{ #category : #primitives }
PluginBasedJPEGReadWriter >> primJPEGWriteImage: aJPEGCompressStruct onByteArray: destination form: form quality: quality progressiveJPEG: progressiveFlag errorMgr: aJPEGErrorMgr2Struct [

	<primitive: 'primJPEGWriteImageonByteArrayformqualityprogressiveJPEGerrorMgr' module: 'JPEGReadWriter2Plugin'>
	self primitiveFailed

]

{ #category : #'public access' }
PluginBasedJPEGReadWriter >> uncompress: aByteArray into: aForm [ 
	"Uncompress an image from the given ByteArray into the given Form.
	Fails if the given Form has the wrong dimensions or depth.
	If aForm has depth 16, do ordered dithering."
	| jpegDecompressStruct jpegErrorMgr2Struct w h |
	aForm unhibernate.
	jpegDecompressStruct := ByteArray new: self primJPEGDecompressStructSize.
	jpegErrorMgr2Struct := ByteArray new: self primJPEGErrorMgr2StructSize.
	self 
		primJPEGReadHeader: jpegDecompressStruct
		fromByteArray: aByteArray
		errorMgr: jpegErrorMgr2Struct.
	w := self primImageWidth: jpegDecompressStruct.
	h := self primImageHeight: jpegDecompressStruct.
	aForm width = w & (aForm height = h) ifFalse: [ ^ self error: 'form dimensions do not match' ].

	"odd width images of depth 16 give problems; avoid them"
	w odd 
		ifTrue: 
			[ aForm depth = 32 ifFalse: [ ^ self error: 'must use depth 32 with odd width' ] ]
		ifFalse: 
			[ aForm depth = 16 | (aForm depth = 32) ifFalse: [ ^ self error: 'must use depth 16 or 32' ] ].
	self 
		primJPEGReadImage: jpegDecompressStruct
		fromByteArray: aByteArray
		onForm: aForm
		doDithering: true
		errorMgr: jpegErrorMgr2Struct
]

{ #category : #'public access' }
PluginBasedJPEGReadWriter >> uncompress: aByteArray into: aForm doDithering: ditherFlag [ 
	"Uncompress an image from the given ByteArray into the given Form. 
	Fails if the given Form has the wrong dimensions or depth.
	If aForm has depth 16 and ditherFlag = true, do ordered dithering."
	| jpegDecompressStruct jpegErrorMgr2Struct w h |
	aForm unhibernate.
	jpegDecompressStruct := ByteArray new: self primJPEGDecompressStructSize.
	jpegErrorMgr2Struct := ByteArray new: self primJPEGErrorMgr2StructSize.
	self 
		primJPEGReadHeader: jpegDecompressStruct
		fromByteArray: aByteArray
		errorMgr: jpegErrorMgr2Struct.
	w := self primImageWidth: jpegDecompressStruct.
	h := self primImageHeight: jpegDecompressStruct.
	aForm width = w & (aForm height = h) ifFalse: [ ^ self error: 'form dimensions do not match' ].

	"odd width images of depth 16 give problems; avoid them"
	w odd 
		ifTrue: 
			[ aForm depth = 32 ifFalse: [ ^ self error: 'must use depth 32 with odd width' ] ]
		ifFalse: 
			[ aForm depth = 16 | (aForm depth = 32) ifFalse: [ ^ self error: 'must use depth 16 or 32' ] ].
	self 
		primJPEGReadImage: jpegDecompressStruct
		fromByteArray: aByteArray
		onForm: aForm
		doDithering: ditherFlag
		errorMgr: jpegErrorMgr2Struct
]

{ #category : #testing }
PluginBasedJPEGReadWriter >> understandsImageFormat [
	"Answer true if the image stream format is understood by this decoder."
	self isPluginPresent ifFalse:[^false]. "cannot read it otherwise"
	self next = 16rFF ifFalse: [^ false].
	self next = 16rD8 ifFalse: [^ false].
	^ true

]
