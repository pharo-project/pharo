user interface
chooseFilePath
	| result |
	result := self theme fileOpenIn: World title: 'Choose a file' extensions: nil path: nil preview: true.
	result 
		ifNotNil: [
			self realValue: result name. 
			result close]