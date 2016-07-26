testing
matchesExtension: aExtension
	(self extension isNil or: [aExtension isNil])
		ifTrue: [^false].
	FileSystem disk isCaseSensitive
		ifTrue: [^extension = aExtension]
		ifFalse: [^extension sameAs: aExtension]