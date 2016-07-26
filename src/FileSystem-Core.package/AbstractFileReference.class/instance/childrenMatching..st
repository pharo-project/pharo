enumerating
childrenMatching: patterns
	"
	FileSystem disk workingDirectory childrenMatching: '*'
	FileSystem disk workingDirectory childrenMatching: '*.image;*.changes'
	"
	^ self 
		childGeneratorBlock: [:reference : aBlock| 
			reference fileSystem childNamesAt: reference path do: aBlock ]
		matching:  patterns 