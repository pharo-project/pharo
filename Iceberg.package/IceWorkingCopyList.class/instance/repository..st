initialization
repository: aGitRepository
	repository := aGitRepository.
	list items: (repository workingCopies
		sorted: [ :a :b | a name < b name ]).
