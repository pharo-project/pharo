tests
testCannotCreateDictionaryRepositoryFromUrl
	self
		should: [ MCRepository fromUrl: 'dict://foo' ]
		raise: MCRepositoryError