public access
imageExtent: aByteArray 
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