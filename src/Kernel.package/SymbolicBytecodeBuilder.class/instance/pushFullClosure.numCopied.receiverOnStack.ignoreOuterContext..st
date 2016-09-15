instruction decoding
pushFullClosure: lit numCopied: numCopied receiverOnStack: rcvr ignoreOuterContext: ignore
	self addBytecode: 'fullClosure:' , lit printString , 'NumCopied: ', numCopied printString