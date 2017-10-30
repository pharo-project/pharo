accessing
currentWorkingDirectoryPathWithBufferSize: aBufferSize
	| buffer |
	buffer := ByteString new: aBufferSize.
	^ self currentWorkingDirectoryPathWithBuffer: buffer