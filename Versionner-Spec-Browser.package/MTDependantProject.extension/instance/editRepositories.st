*Versionner-Spec-Browser
editRepositories
	| packageName repoManager |
	
	packageName := configurationClass package name.
	repoManager := VersionnerRepositoriesManager newWithPackage: packageName.
	repoManager
		selectedChangedBlock:  [:remote :selected |
			selected
				ifTrue: [ self repositories add: remote location ]
				ifFalse: [ self repositories remove: remote location ] ];
		open;
		yourself.