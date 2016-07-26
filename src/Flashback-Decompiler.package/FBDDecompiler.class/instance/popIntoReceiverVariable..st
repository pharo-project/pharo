data flow instructions
popIntoReceiverVariable: offset 
	self storeIntoReceiverVariable: offset.
	self doPop.
