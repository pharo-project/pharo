reporting
reportRepositories: aStream
	self header: 'Monticello Repositories' on: aStream.
	MCRepositoryGroup default repositories do: [:each | aStream nextPutAll: each description; cr]