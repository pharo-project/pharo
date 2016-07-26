initialization
initializeType: aType size: aNumber
	self 
		initializeType: aType 
		size: aNumber 
		handleProducer: [ :totalSize | ByteArray new: totalSize ]
