public access
uncompress: aByteArray into: aForm 
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