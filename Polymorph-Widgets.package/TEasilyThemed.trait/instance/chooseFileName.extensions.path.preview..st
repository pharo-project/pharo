services
chooseFileName: title extensions: exts path: path preview: preview
	"Answer the result of a file name chooser dialog with the given title, extensions
	to show, path and preview type."

	^self theme
		chooseFileNameIn: self
		title: title
		extensions: exts
		path: path
		preview: preview