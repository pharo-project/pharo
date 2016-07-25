services
fileSave: title extensions: exts path: path
	"Answer the result of a file save dialog with the given title, extensions to show and path."

	^self theme
		fileSaveIn: self
		title: title
		extensions: exts
		path: path