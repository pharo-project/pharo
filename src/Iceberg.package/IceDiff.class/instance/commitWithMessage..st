actions
commitWithMessage: aCommitMessage
	self elements do: [ :package |
		package hasChanges 
			ifTrue: [ 
				"It should not be necessary to hand the repository to the package change set"
				package commitWithMessage: aCommitMessage to: self repository.
				self refresh. ]]