instance creation
fieldName: fieldName fileNamed: fileName
	| mimeType size fileStream entity baseName |
	mimeType := ZnMimeType forFilenameExtension: (ZnFileSystemUtils extensionFor: fileName).
	fileStream := ZnFileSystemUtils oldFileStreamFor: fileName.
	mimeType isBinary ifTrue: [ fileStream binary ].
	size := ZnFileSystemUtils fileSizeFor: fileName.
	(entity := ZnStreamingEntity type: mimeType length: size)
		stream: fileStream.
	baseName := ZnFileSystemUtils baseNameFor: fileName.
	^ self fieldName: fieldName fileName: baseName entity: entity
	