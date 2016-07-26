selecting
protocolSelection
	^ protocolSelection 
		ifNil: [0]
		ifNotNil: [self visibleProtocols indexOf: protocolSelection]