accessing
repositoryURLToNameMappings
	^ repositoryURLToNameMappings ifNil: [ repositoryURLToNameMappings := self createRepositoryURLtoNameMappings ]