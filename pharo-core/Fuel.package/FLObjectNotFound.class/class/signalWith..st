signaling
signalWith: anObject

 	self signal: (
		String streamContents: [ :stream |
			stream << 'Unexpected reference to '.
			stream print: anObject.
			stream << ' in the graph. This usually happens when the graph changes during serialization.'.
			]
	)