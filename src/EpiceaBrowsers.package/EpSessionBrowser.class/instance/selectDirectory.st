accessing
selectDirectory

	^ (UIManager default chooseDirectoryFrom: self directory)
		ifNotNil: [ :selectedDirectory | self directory: selectedDirectory ]