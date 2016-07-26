services
chooseDirectory: title path: path
	"Answer the result of a file dialog with the given title, answer a directory."

	^self theme
		chooseDirectoryIn: self
		title: title
		path: path