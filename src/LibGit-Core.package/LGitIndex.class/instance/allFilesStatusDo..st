enumerating
allFilesStatusDo: aBlock
	"The callback is passed the path of the file, the status (a combination of the git_status_t values above) and the payload data pointer passed into this function.

If the callback returns a non-zero value, this function will stop looping and return that value to caller."
	self withReturnHandlerDo: [ 
		self
			status_foreach: self repository
			callback: (LGitStatusCallback on: aBlock)
			payload: nil ]