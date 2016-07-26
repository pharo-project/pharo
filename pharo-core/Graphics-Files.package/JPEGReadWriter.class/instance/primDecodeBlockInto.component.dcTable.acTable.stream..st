huffman encoding
primDecodeBlockInto: sampleBuffer component: comp dcTable: dcTable acTable: acTable stream: jpegStream
	<primitive: 'primitiveDecodeMCU' module: 'JPEGReaderPlugin'>
	^self decodeBlockInto: sampleBuffer component: comp dcTable: dcTable acTable: acTable