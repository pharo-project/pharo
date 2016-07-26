instruction decoding
popIntoLiteralVariable: offset
	self storeIntoLiteralVariable: offset.
	self doPop.