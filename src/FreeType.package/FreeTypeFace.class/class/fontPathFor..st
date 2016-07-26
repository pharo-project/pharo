font dirs
fontPathFor: aFilename
	"aFilename is local. Try hard to return a valid path to be handed to freetype library"
	
	"temporary solution ;-)"
	^ (FileSystem disk workingDirectory / 'Fonts' / aFilename) fullName