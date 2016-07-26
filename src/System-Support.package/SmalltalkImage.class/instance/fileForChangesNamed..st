image, changes name
fileForChangesNamed: aString

	^(self imageDirectory resolve: aString), self changesSuffix