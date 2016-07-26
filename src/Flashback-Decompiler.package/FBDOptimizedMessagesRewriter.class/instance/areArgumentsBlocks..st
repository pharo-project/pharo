testing
areArgumentsBlocks: msgNode
	^ msgNode arguments allSatisfy: [ :arg | arg isBlock ]