initialization
initializeExternalType: aType size: aNumber
	self 
		initializeType: aType 
		size: aNumber 
		handleProducer: [ :totalSize | ExternalAddress allocate: (type typeSize * size) ]