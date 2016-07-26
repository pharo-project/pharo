tests
testCanCreateFtpRepositoryFromUrl
	repository := MCRepository fromUrl: 'ftp://ftp.example.com/examples'.
	self assert: (repository isKindOf: MCFtpRepository) 