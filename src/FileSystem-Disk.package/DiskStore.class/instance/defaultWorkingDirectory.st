accessing
defaultWorkingDirectory
	"Answer the default working directory, which is defined as the directory where the image resides."

	^ DefaultWorkingDirectory
		ifNil: [ self class defaultWorkingDirectory ]