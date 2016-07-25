accessing
selectedProtocol
	^ self selectedCategory 
		ifNotNil: [ :protocolName | self selectedClass organization protocolNamed: protocolName ]