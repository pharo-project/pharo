converting
currentDisk
	^ disk ifNil: [  disk := FileSystem workingDirectory path segments first ]