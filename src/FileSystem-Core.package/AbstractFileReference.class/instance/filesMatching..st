enumerating
filesMatching: patterns
	"
	FileSystem disk workingDirectory filesMatching: '*'
	FileSystem disk workingDirectory filesMatching: '*.image;*.changes'
	"
	^ self 
		childGeneratorBlock: [:reference : aBlock| 
			reference fileSystem fileNamesAt: reference path do: aBlock ]
		matching:  patterns 