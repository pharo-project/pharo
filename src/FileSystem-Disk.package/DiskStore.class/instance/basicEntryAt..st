public
basicEntryAt: aPath
	| encodedPath encodedBasename |
	
	encodedPath := Primitives encode: (self stringFromPath: aPath parent).
	encodedBasename := Primitives encode: aPath basename.
	
	^ (Primitives lookupDirectory: encodedPath filename: encodedBasename)
		ifNil: [ #badDirectoryPath ].
	