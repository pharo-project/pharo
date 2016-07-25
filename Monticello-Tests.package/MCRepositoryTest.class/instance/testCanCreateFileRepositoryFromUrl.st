tests
testCanCreateFileRepositoryFromUrl
	repository := MCRepository fromUrl: 'file:///tmp'.
	self assert: (repository isKindOf: MCFileBasedRepository) 