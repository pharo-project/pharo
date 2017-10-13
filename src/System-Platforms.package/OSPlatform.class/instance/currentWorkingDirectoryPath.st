accessing
currentWorkingDirectoryPath
	"This method calls the method getPwdViaFFI with arugement of a buffer size. By default it uses the defaultMaximumPathLength of each subclass as the buffer size."
	^ self currentWorkingDirectoryPathWithBufferSize: self defaultMaximumPathLength
