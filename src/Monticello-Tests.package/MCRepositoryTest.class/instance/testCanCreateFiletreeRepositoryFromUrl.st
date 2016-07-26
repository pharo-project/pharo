tests
testCanCreateFiletreeRepositoryFromUrl
	repository := MCRepository fromUrl: (self homeDirectoryUrl scheme: 'filetree') asString.
	self assert: (repository isKindOf: MCFileTreeRepository) 