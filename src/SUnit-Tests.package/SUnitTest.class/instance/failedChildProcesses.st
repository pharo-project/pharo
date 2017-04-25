helpers
failedChildProcesses
	^Process allInstances 
		select: [: each | each name = ('failed child for ', testSelector)]