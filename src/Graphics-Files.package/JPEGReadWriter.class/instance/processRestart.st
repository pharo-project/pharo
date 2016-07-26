huffman encoding
processRestart
	stream resetBitBuffer.
	self parseNextMarker.
	currentComponents do: [ :c | c priorDCValue: 0 ].
	restartsToGo := restartInterval