accessing
fileSystem
	^ reference 
		ifNil: [ FileSystem disk ]
		ifNotNil: [ reference fileSystem ]