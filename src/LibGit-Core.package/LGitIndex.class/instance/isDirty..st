testing
isDirty: aFileReference
	| intValue |
	intValue := LGitWriteBackValueInt new.
	self withReturnHandlerDo: [
		self
			status_file: intValue
			repo: self repository
			path: ((aFileReference relativeTo: self repository repositoryReference) printWithDelimiter: $/) ].
		
	^ intValue value isZero not