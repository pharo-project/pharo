tests
testCanCreateHttpRepositoryFromUrl
	repository := MCRepository fromUrl: 'http://mydomain.com'.
	self assert: (repository isKindOf: MCHttpRepository) 