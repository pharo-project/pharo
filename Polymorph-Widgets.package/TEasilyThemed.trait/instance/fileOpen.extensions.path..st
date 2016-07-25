services
fileOpen: title extensions: exts path: path
	"Answer the result of a file open dialog with the given title, extensions to show and path."

	^self 
		fileOpen: title
		extensions: exts
		path: path
		preview: nil