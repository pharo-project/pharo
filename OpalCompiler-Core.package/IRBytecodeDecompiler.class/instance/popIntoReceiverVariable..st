instruction decoding
popIntoReceiverVariable: offset
	self storeIntoReceiverVariable: offset.
	self doPop.