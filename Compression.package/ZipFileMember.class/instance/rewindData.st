private-reading
rewindData
	super rewindData.
	(stream isNil or: [ stream closed ])
		ifTrue: [ self error: 'stream missing or closed' ].
	stream position: (localHeaderRelativeOffset + 4).
	self skipLocalDirectoryFileHeaderFrom: stream.