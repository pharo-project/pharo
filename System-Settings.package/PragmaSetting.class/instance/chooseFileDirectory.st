user interface
chooseFileDirectory
	| result |
	result := self theme chooseDirectoryIn: World title: 'Choose a file' path: nil.
	result ifNotNil: [ self realValue: result fullName ].