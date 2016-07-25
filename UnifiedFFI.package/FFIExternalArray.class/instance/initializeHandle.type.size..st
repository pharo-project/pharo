initialization
initializeHandle: aHandle type: aType size: aNumber
	self 
		initializeType: aType 
		size: aNumber 
		handleProducer: [ :totalSize | aHandle ]