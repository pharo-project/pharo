image, changes name
sourcesFile
	"Answer the full path to the version-stable source code"
	^ self imageDirectory / (SourceFileVersionString , '.', self sourcesSuffix)