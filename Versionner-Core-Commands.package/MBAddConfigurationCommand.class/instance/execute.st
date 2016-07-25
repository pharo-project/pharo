execute
execute
	| configurationClassName versionNumber addedPackages addedConfigurations repository projectName mcPackage workingCopy configInfo|
	configurationClassName := self askForClassName.
	configurationClassName ifNil: [ ^ self ].
	Smalltalk globals
		at: configurationClassName asSymbol
		ifPresent: [ :ex | ^ self error: 'Class ' , configurationClassName printString , ' already exist' ].
	projectName := configurationClassName name configurationBaseName.
	(repository := self repositoryFor: projectName) ifNil: [ ^ self ].

	versionNumber := self uiManager request: 'Enter initial version number' 
										     initialAnswer: '1.0'.
	versionNumber ifNil: [ ^ self ].
	addedPackages := OrderedCollection new.
	addedConfigurations := OrderedCollection new.
	
	self collectPackages: addedPackages
		 andProjects: addedConfigurations 
		 pattern: projectName , '*' 
		 repository: repository.
	self
		executeAddBaselineForConfigurationNamed: configurationClassName
		repositoryDescription: repository description
		versionString: versionNumber
		packages: addedPackages
		configurations: addedConfigurations.
	
	mcPackage := MCPackage named: configurationClassName.
	workingCopy := mcPackage workingCopy.
	workingCopy repositoryGroup addRepository: repository.
	"after all save configuration"
	configInfo := (MBConfigurationInfo 
						configurationClass: (Smalltalk globals at: (configurationClassName asSymbol)))
						configurationRoot: (MBConfigurationRoot current);
						yourself.
	"fixing"
	(MBSaveConfigurationCommand 
				target: configInfo 
				for: requestor) executeWithMessage: 'Creating ConfigurationOf', projectName.
	