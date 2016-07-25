repositories
repositoryOfPackageNamed: packageName
	"Return the repository of the package given as parameter. Create one (using UI) if necessary"

	| mcPackage workingCopy repositories repository repositoryIndex |
	mcPackage := MCPackage named: packageName.
	workingCopy := mcPackage workingCopy.
	repositories := workingCopy repositoryGroup repositories reject: [ :rep | rep == MCCacheRepository default ].
	repositories
		ifEmpty: [ repository := MCTool newRepository.
				   workingCopy repositoryGroup addRepository: repository. ].
	repositories size > 1
		ifTrue: [ 
			repositoryIndex := self uiManager chooseFrom: (repositories collect: #printString).
			repositoryIndex = 0
				ifTrue: [ ^ nil ].
			repository := repositories at: repositoryIndex ].
	repositories size = 1
		ifTrue: [ repository := repositories first ].
	^ repository