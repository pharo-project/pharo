testing
isInPath: urlObject
	^ urlObject pathPrintString includesSubstring: self path
	