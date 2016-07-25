actions
updateRepository
	"Update the core repository of this project"
	| package repoManager |
	
	project ifNotNil: [ 
		package := project configurationClass package mcPackage.
		repoManager := VersionnerRepositoriesManager new.
		repoManager
			selectedChangedBlock:  [:remote :selected |
				selected
					ifTrue: [ | repositoryGroup | 
							repositoryGroup := package workingCopy repositoryGroup.
							repositoryGroup repositoriesDo: [ :aRepository | 
									(aRepository location = repository text) 
										ifTrue: [ repositoryGroup removeRepository: aRepository ] ].
							repository accept: remote location.
							repositoryGroup addRepository: remote ] ];
			open;
			yourself  ]