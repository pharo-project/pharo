accessing
terminfo
	^ terminfo
		ifNil: [ self terminfo: (OSPlatform current newTerminfo on: self outStream) ]