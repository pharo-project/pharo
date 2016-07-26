writing
nextPutImage: aForm interlace: aMethod filter: aFilterType 
	"Note: For now we keep it simple - interlace and filtering are simply ignored"

	| crcStream |
	bigEndian := EndianDetector isBigEndian.
	form := aForm.
	width := aForm width.
	height := aForm height.
	aForm depth <= 8 
		ifTrue: 
			[bitsPerChannel := aForm depth.
			colorType := 3.
			bytesPerScanline := (width * aForm depth + 7) // 8]
		ifFalse: 
			[bitsPerChannel := 8.
			colorType := 6.
			bytesPerScanline := width * 4].
	self writeFileSignature.
	crcStream := (ByteArray new: 1000) writeStream.
	crcStream resetToStart.
	self writeIHDRChunkOn: crcStream.
	self writeChunk: crcStream.
	form depth <= 8 
		ifTrue: 
			[crcStream resetToStart.
			self writePLTEChunkOn: crcStream.
			self writeChunk: crcStream.
			form isColorForm 
				ifTrue: 
					[crcStream resetToStart.
					self writeTRNSChunkOn: crcStream.
					self writeChunk: crcStream]].
	form depth = 16 
		ifTrue: 
			[crcStream resetToStart.
			self writeSBITChunkOn: crcStream.
			self writeChunk: crcStream].
	crcStream resetToStart.
	self writeIDATChunkOn: crcStream.
	self writeChunk: crcStream.
	crcStream resetToStart.
	self writeIENDChunkOn: crcStream.
	self writeChunk: crcStream