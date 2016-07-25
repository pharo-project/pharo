services
fileOpen: title extensions: exts path: path preview: preview
	"Answer the result of a file open dialog with the given title, extensions to show, path and preview type."

	^self theme
		fileOpenIn: self
		title: title
		extensions: exts
		path: path
		preview: preview