private-navigation
subclassesOf: aClass filterBySubstring: aString inPackage: aPackageName
	^ (self subclassesOf: aClass inPackage: aPackageName) select: [ :each |
		each name includesSubstring: aString caseSensitive: false ]