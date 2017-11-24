"
Command to add a configuration in Versionner
"
Class {
	#name : #MBAddConfigurationCommand,
	#superclass : #MBDevCycleCommands,
	#category : #'Versionner-Core-Commands'
}

{ #category : #execute }
MBAddConfigurationCommand >> askForClassName [
	^ self uiManager
		request: 'Name of the new Metacello configuration (e.g., ConfigurationOfYourSoftware)' translated
		initialAnswer: 'ConfigurationOf'
]

{ #category : #private }
MBAddConfigurationCommand >> collectPackages: addedPackages andProjects: addedConfigurations pattern: patternString repository: repository [
	| packagePattern package |
	packagePattern := self  uiManager
		request: 'Please enter a pattern to filter package names that will be candidates to add to configuration'
		initialAnswer: patternString.
	packagePattern ifNil: [ ^ self ].
	[ 
	| packageNames workingCopies |
	packageNames := OrderedCollection new.
	workingCopies := OrderedCollection new.
	self workingCopies
		do: [ :pkg | 
			((packagePattern match: pkg package name) and: [ (addedPackages includes: pkg package name) not ])
				ifTrue: [ 
					packageNames add: pkg package name.
					workingCopies add: pkg ] ].
	package := self uiManager
		chooseFrom: packageNames
		values: workingCopies
		title: 'Add dependent package (cancel to stop)'.
	package
		ifNotNil: [ 
			(package package name beginsWith: 'ConfigurationOf')
				ifTrue: [ addedConfigurations add: package package name ]
				ifFalse: [ addedPackages add: package package name ].
			package ancestry ancestors isEmpty
				ifTrue: [ 
					(self uiManager
						confirm:
							'The package: ' , package package name printString , ' has not been committed yet. Would you like to commit it into the '
								, repository description printString, ' repository?')
						ifTrue: [ 
							package repositoryGroup repositories size <= 1
								ifTrue: [ package repositoryGroup addRepository: repository ].
							package currentVersionInfo ] ] ].
	package notNil ] whileTrue.
	(addedPackages isEmpty and: [ addedConfigurations isEmpty ])
		ifTrue: [ self uiManager inform: 'No package or configuration added. No baseline has been created' ]
]

{ #category : #documentation }
MBAddConfigurationCommand >> documentation [
	^ 'Create a new configuration. Prompt for packages and dependent projects. Create a baseline version and an initial development version'
]

{ #category : #execute }
MBAddConfigurationCommand >> execute [
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
	
]

{ #category : #execute }
MBAddConfigurationCommand >> executeAddBaselineForConfigurationNamed: aConfigurationClassName repositoryDescription: repositoryDescription versionString: versionString packages: packageNames configurations: configurationNames [
	| baselineVersionString |
	baselineVersionString := versionString , '-baseline'.
	MetacelloToolBox
		createBaseline: baselineVersionString
			for: aConfigurationClassName
			repository: repositoryDescription
			requiredProjects: configurationNames
			packages: packageNames
			repositories: { aConfigurationClassName, repositoryDescription }
			dependencies: {}
			groups: {('default' -> packageNames)};
		createDevelopment: versionString
			for: aConfigurationClassName
			importFromBaseline: baselineVersionString
			description: ''
]

{ #category : #execute }
MBAddConfigurationCommand >> executeAddBaselineForConfigurationNamed: aConfigurationClassName repositoryDescription: repositoryDescription versionString: versionString packages: packageNames configurations: configurationNames repositories: repositories [
	| baselineVersionString |
	baselineVersionString := versionString , '-baseline'.
	MetacelloToolBox
		createBaseline: baselineVersionString
			for: aConfigurationClassName
			repository: repositoryDescription
			requiredProjects: configurationNames
			packages: packageNames
			repositories: repositories
			dependencies: {}
			groups: {('default' -> packageNames)};
		createDevelopment: versionString
			for: aConfigurationClassName
			importFromBaseline: baselineVersionString
			description: ''
]

{ #category : #ordering }
MBAddConfigurationCommand >> order [

	^'20'
]

{ #category : #private }
MBAddConfigurationCommand >> repositoryFor: projectName [
	| repositories repository |
	repositories := self repositories
		select: [ :repo | (repo description beginsWith: 'http:') and: [ repo description endsWith: projectName ] ].
	repository := nil.
	(repositories anySatisfy: [ :http | http description = ('http://www.squeaksource.com/' , projectName) ])
		ifFalse: [ 
			(self uiManager confirm: 'Would you like to use http://www.squeaksource.com/' , projectName , ', for your project?')
				ifTrue: [ ^ MCHttpRepository location: 'http://www.squeaksource.com/' , projectName user: '' password: '' ] ].
	repository isNil
		ifTrue: [ 
			repository := repositories isEmpty
				ifTrue: [ self chooseRepositoryFromList: self repositories ]
				ifFalse: [ self chooseRepositoryFromList: repositories , self repositories ] ].
	^ repository
]

{ #category : #accessing }
MBAddConfigurationCommand >> title [
	^ 'add configuration'
]

{ #category : #private }
MBAddConfigurationCommand >> workingCopies [
	^ MCWorkingCopy allManagers asSortedCollection: [ :a :b | a name <= b name]
]
