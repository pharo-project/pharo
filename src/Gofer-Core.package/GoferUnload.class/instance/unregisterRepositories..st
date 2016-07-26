unregistering
unregisterRepositories: aWorkingCopy
	aWorkingCopy repositoryGroup repositories allButFirst do: [ :repository |
		MCWorkingCopy allManagers do: [ :copy |
			(copy repositoryGroup includes: repository)
				ifTrue: [ ^ self ] ].
		MCRepositoryGroup default
			removeRepository: repository ]