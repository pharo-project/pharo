tests
testCanCreateGitHubRepositoryFromUrl
	repository := MCRepository fromUrl: 'github://dalehenrich/MetacelloRepository:master/monticello/repos/itory/path'.
	self assert: (repository isKindOf: MCGitHubRepository) 