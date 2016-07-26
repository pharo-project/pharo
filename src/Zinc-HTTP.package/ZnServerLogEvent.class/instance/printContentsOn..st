printing
printContentsOn: stream
	super printContentsOn: stream.
	serverId ifNotNil: [ stream space; << serverId ].
	stream space.
	processId printOn: stream base: 10 length: 6 padded: true