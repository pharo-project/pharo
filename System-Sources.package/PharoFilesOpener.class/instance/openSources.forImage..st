open sources
openSources: fullSourcesName forImage: imagePath
	"Look in various places for a sources file, return an open stream to it."

	| sourcesFile sourcesName |
	sourcesFile := fullSourcesName asFileReference.
	sourcesName := sourcesFile basename.
	
	"look for the sources file or an alias to it in the VM's directory"
	OSPlatform current potentialLocationsOfSourcesFile do: [ :dir |
		self ignoreIfFail: [ 
			(sourcesFile := dir / sourcesName) exists
				ifTrue: [ sourcesFile readStream ifNotNil: [ :stream | ^ stream ] ] ] ].
			
	"look for the sources file or an alias to it in the image directory"
	(sourcesFile := imagePath asFileReference parent / sourcesName) exists
		ifTrue: [ ^ sourcesFile readStream ifNotNil: [ :stream | ^ stream ] ].
		
	"look for the sources in the current directory"
	(sourcesFile := sourcesName asFileReference) exists
		ifTrue: [ ^ sourcesFile readStream ifNotNil: [ :stream | ^ stream ] ].
		
	^ nil