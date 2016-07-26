chunks
processNextChunk
	| length chunkType crc chunkCrc |
	length := self nextLong.
	chunkType := (self next: 4) asString.
 	(chunk isNil or: [ chunk size ~= length ])
 		ifTrue: [ chunk := self next: length ]
 		ifFalse: [ stream next: length into: chunk startingAt: 1 ].
	chunkCrc := self nextLong bitXor: 4294967295.
	crc := self 
		updateCrc: 4294967295
		from: 1
		to: 4
		in: chunkType.
	crc := self 
		updateCrc: crc
		from: 1
		to: length
		in: chunk.
	crc = chunkCrc ifFalse: [ self error: 'PNGReadWriter crc error in chunk ' , chunkType ].
	chunkType = 'IEND' ifTrue: [ ^ self	"*should* be the last chunk" ].
	chunkType = 'sBIT' ifTrue: 
		[ ^ self processSBITChunk	"could indicate unusual sample depth in original" ].
	chunkType = 'gAMA' ifTrue: [ ^ self	"indicates gamma correction value" ].
	chunkType = 'bKGD' ifTrue: [ ^ self processBackgroundChunk ].
	chunkType = 'pHYs' ifTrue: [ ^ self processPhysicalPixelChunk ].
	chunkType = 'tRNS' ifTrue: [ ^ self processTransparencyChunk ].
	chunkType = 'IHDR' ifTrue: [ ^ self processIHDRChunk ].
	chunkType = 'PLTE' ifTrue: [ ^ self processPLTEChunk ].
	chunkType = 'IDAT' ifTrue: 
		[ "---since the compressed data can span multiple
		chunks, stitch them all together first. later,
		if memory is an issue, we need to figure out how
		to do this on the fly---"
 		idatChunkStream
 			ifNil: [ idatChunkStream := WriteStream with: chunk copy ]
 			ifNotNil: [ idatChunkStream nextPutAll: chunk ].
		^ self ].
	unknownChunks add: chunkType