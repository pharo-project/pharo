instruction decoding
popIntoReceiverVariable: offset

	self pushReceiverVariable: offset; doStore: statements