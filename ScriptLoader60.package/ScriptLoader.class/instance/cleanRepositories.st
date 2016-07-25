cleaning
cleanRepositories
	| workingCopies |
	"self new cleanRepositories"
	MCRepositoryGroup instVarNamed: 'default' put: nil. 
	self allManagers do: [ :each | each repositoryGroup: nil ].
	workingCopies := self addRepositoryToPackagesWithConfiguration.
	self addHomeRepositoryToAllPackagesExcept: workingCopies