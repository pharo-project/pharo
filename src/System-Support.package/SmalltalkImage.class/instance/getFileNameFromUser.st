snapshot and quit
getFileNameFromUser

	| givenName strippedName imageFile changesFile |
	givenName := UIManager default
		request: 'New File Name?' translated
		initialAnswer: (self imageFile basename copyUpToLast:  Path extensionDelimiter).
	strippedName := self stripImageExtensionFrom: givenName.
	strippedName isEmptyOrNil ifTrue: [^nil].
	imageFile := self fileForImageNamed: strippedName.
	changesFile := self fileForChangesNamed: strippedName.
	((imageFile exists or: [changesFile exists]) and:
		[(self confirm: ('{1} already exists. Overwrite?' translated format: {strippedName})) not])
			ifTrue: [^nil].
	^strippedName