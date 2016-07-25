repositories
repositoryOfPackageNamed: packageName
	"Return the repository of the configuration. Create one (using UI) if necessary"

	| mcPackage workingCopy repositories repository repositoryIndex username password |
	mcPackage := MCPackage named: packageName.
	workingCopy := mcPackage workingCopy.
	repositories := workingCopy repositoryGroup repositories reject: [ :rep | rep == MCCacheRepository default ].
	repositories
		ifEmpty: [ 
			| projectName answer squeakSourceURL |
			projectName := self configurationClass name configurationBaseName.
			squeakSourceURL := 'http://www.squeaksource.com/' , projectName.
			answer := self uiManager
				confirm: 'No repository associated with your project. Do you want to add ' , squeakSourceURL.
			answer
				ifFalse: [ 
					self uiManager  inform: 'Add a http repository with Monticello browser'.
					^ nil ].
			repository := MCHttpRepository location: squeakSourceURL user: '' password: ''.
			repository user isEmpty
				ifTrue: [ username := self username ].
			repository password isEmpty
				ifTrue: [ password := self password ].
			workingCopy repositoryGroup
				addRepository: (repository := MCHttpRepository location: squeakSourceURL user: username password: password) ].
	repositories size > 1
		ifTrue: [ 
			repositoryIndex := self uiManager chooseFrom: (repositories collect: #printString).
			repositoryIndex = 0
				ifTrue: [ ^ nil ].
			repository := repositories at: repositoryIndex ].
	repositories size = 1
		ifTrue: [ repository := repositories first ].
	^ repository