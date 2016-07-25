accessing
path
	^ attributes at: 'path' ifAbsent: [ '/' ]