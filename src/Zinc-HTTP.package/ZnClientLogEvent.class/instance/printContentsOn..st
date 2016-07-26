printing
printContentsOn: stream
	super printContentsOn: stream.
	clientId ifNotNil: [ stream space; << clientId ]