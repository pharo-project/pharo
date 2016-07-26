instruction decoding
pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize
	| copiedValues |
	self sawClosureBytecode.
	copiedValues := ((1 to: numCopied) collect: [:ign| stack removeLast]) reversed.
	self doClosureCopyCopiedValues: copiedValues numArgs: numArgs blockSize: blockSize