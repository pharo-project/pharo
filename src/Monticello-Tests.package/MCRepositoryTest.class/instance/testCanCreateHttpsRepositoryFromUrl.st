tests
testCanCreateHttpsRepositoryFromUrl
	repository := MCRepository fromUrl: 'https://mydomain.com'.
	self assert: (repository isKindOf: MCHttpRepository) 