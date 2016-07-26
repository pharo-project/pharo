accessing
localFileName: aString
	"Set my internal filename.
	Returns the (possibly new) filename"

	^fileName := aString copyReplaceAll: FileSystem disk delimiter asString with: UnixStore delimiter asString.