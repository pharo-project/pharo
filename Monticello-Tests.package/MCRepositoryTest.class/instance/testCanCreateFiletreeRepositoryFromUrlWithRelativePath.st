tests
testCanCreateFiletreeRepositoryFromUrlWithRelativePath
	repository := MCRepository fromUrl: 'filetree://.' asUrl.
	self assert: (repository isKindOf: MCFileTreeRepository) 