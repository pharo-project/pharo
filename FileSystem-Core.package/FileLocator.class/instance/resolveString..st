navigating
resolveString: aString
	| filesystem thePath |
	filesystem := (Resolver resolve: origin) fileSystem.
	thePath := filesystem pathFromString: aString.
	^ self withPath: (path resolvePath: thePath)