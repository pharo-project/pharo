instruction decoding
storeIntoReceiverVariable: offset

	self pushReceiverVariable: offset; doStore: stack