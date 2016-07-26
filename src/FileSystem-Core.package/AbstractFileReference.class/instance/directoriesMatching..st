enumerating
directoriesMatching: patterns
	"
	FileSystem disk workingDirectory directoriesMatching: '*'
	FileSystem disk workingDirectory directoriesMatching: 'package-cache'
	"
	^ self 
		childGeneratorBlock: [:reference : aBlock| 
			reference fileSystem directoryNamesAt: reference path do: aBlock ]
		matching:  patterns 