accessing
currentWorkingDirectoryPathWithBufferSize: aBufferSize
	"This method calls the method getPwdViaFFI with arugement of a buffer size."
	|buffer |
	buffer := String new: aBufferSize  .
	^self getPwdViaFFI: buffer size:aBufferSize. 
