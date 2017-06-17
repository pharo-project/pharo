testing
isBeingTracked: aString
	| intValue |
	(self repository repositoryReference resolve: aString) isDirectory ifTrue: [ ^ true ].
	
	intValue := LGitWriteBackValueInt new.
	self withReturnHandlerDo: [ 
		self
			status_file: intValue
			repo: self repository
			path: aString ].

	^ (LGitStatusTypeEnum  git_status_wt_new isPartOfBitFlag: intValue value) not