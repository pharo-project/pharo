actions
snapshot
	| version |
	[version := workingCopy newVersionIn: workingCopy repositoryGroup]
		on: MCVersionNameAndMessageRequest
		do: [:n | n resume: (Array with: n suggestedName with: '')].
	versions at: version info put: version.
	^ version