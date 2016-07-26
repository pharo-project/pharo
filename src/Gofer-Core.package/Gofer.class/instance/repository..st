repositories
repository: aRepository
	"Add aRepository to the repository configuration. If there is already a repository defined in the global configuration with that URL take this one instead."
	
	| repository |
	repository := MCRepositoryGroup default repositories
		detect: [ :each | each = aRepository ]
		ifNone: [ aRepository ].
	repositories addLast: repository