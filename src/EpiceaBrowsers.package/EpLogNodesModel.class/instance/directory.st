accessing
directory

	^ directory ifNil: [ 
		directory := EpMonitor current sessionStore baseLocator ]