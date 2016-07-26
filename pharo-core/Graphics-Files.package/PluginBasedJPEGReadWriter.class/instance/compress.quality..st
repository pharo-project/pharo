public access
compress: aForm quality: quality 
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