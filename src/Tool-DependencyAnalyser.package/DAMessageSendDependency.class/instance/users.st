accessing
users
	^  (source rPackage methods
		select: [ :method | method messages includesAny: implementedMethods ]
		thenCollect: [ :method | method -> (method messages intersection: implementedMethods) ])