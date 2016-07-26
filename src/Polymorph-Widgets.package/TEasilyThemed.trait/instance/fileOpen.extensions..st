services
fileOpen: title extensions: exts
	"Answer the result of a file open dialog with the given title and extensions to show."

	^self
		fileOpen: title
		extensions: exts
		path: nil