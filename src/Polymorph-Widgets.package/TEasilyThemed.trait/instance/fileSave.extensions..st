services
fileSave: title extensions: exts
	"Answer the result of a file save dialog with the given title."

	^self
		fileSave: title
		extensions: exts
		path: nil