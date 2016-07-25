actions
saveRepositories

	FileStream forceNewFileNamed: 'MCRepositories.st' do: [:f |
		MCRepositoryGroup default repositoriesDo: [:r |
			r asCreationTemplate ifNotNil: [:template |
				f nextPutAll: 'MCRepositoryGroup default addRepository: (', template , ')!'; cr]]]