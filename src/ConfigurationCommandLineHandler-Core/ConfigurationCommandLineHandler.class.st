"
Command line handler for dealing with Metacello configurations from the command line

Usage: config [--help] <repository url> [<configuration>] [--install[=<version>]] [--group=<group>] [--username=<username>] [--password=<password>][--no-quit][--no-save]
	--help              show this help message
	--no-quit        keep the image running after configuration install
	--no-save       Don't save the image after configuration install
	<repository url>    A Monticello repository name 
	<configuration>     A valid Metacello Configuration name
	<version>           A valid version for the given configuration
	<group>             A valid Metacello group name
	<username>          An optional username to access the configuration's repository
	<password>          An optional password to access the configuration's repository
	
Examples:
	# display this help message
	pharo Pharo.image config
	
	# list all configurations of a repository
	pharo Pharo.image config $MC_REPOS_URL
	
	# list all the available versions of a confgurtation
	pharo Pharo.image config $MC_REPOS_URL ConfigurationOfFoo
	
	# install the stable version
	pharo Pharo.image config $MC_REPOS_URL ConfigurationOfFoo --install
	
	#install a specific version '1.5'
	pharo Pharo.image config $MC_REPOS_URL ConfigurationOfFoo --install=1.5
	
	#install a specific version '1.5' and only a specific group 'Tests'
	pharo Pharo.image config $MC_REPOS_URL ConfigurationOfFoo --install=1.5 --group=Tests

"
Class {
	#name : #ConfigurationCommandLineHandler,
	#superclass : #CommandLineHandler,
	#instVars : [
		'version',
		'repositoryURL',
		'configurationName'
	],
	#category : #'ConfigurationCommandLineHandler-Core'
}

{ #category : #accessing }
ConfigurationCommandLineHandler class >> commandName [
	^ 'config'
]

{ #category : #accessing }
ConfigurationCommandLineHandler class >> description [
	^ 'Install and inspect Metacello Configurations from the command line'
]

{ #category : #activation }
ConfigurationCommandLineHandler >> activate [
	self activateHelpWithoutArguments
		ifTrue: [ ^ self ].
	
	(self hasOption: 'install')
		ifFalse: [ ^ self list ].
		
	self installConfiguration.
	
	self postConfiguration.
		
]

{ #category : #accessing }
ConfigurationCommandLineHandler >> configuration [
	| gofer |
	gofer := self gofer.
	self hasConfiguration 
		ifTrue: [ gofer package: self configurationName ]
		ifFalse: [ gofer configuration ].
	gofer load.
	^ Smalltalk globals at: self configurationName asSymbol
]

{ #category : #accessing }
ConfigurationCommandLineHandler >> configurationName [
	^ configurationName ifNil: [
			self hasConfiguration 
				ifFalse: [ "Demeters favourite line :("
					^ self gofer configuration references last name ].
		 configurationName := self argumentAt: 2 ]
]

{ #category : #accessing }
ConfigurationCommandLineHandler >> configurationVersion [
	^ (self hasOption: 'install')
		ifFalse: [ #stable ]
		ifTrue: [ self optionAt: 'install' ]
]

{ #category : #accessing }
ConfigurationCommandLineHandler >> defaultRepositoryUrl [
	^ 'http://smalltalkhub.com/mc/Pharo/MetaRepoForPharo', SystemVersion current major asString, SystemVersion current minor asString, '/main'
]

{ #category : #actions }
ConfigurationCommandLineHandler >> errorNoConfigurationsFound [

	self exitFailure: 'No Configurations found in ', repositoryURL asString
]

{ #category : #accessing }
ConfigurationCommandLineHandler >> gofer [
	| username password |
	username := (self hasOption: 'username')
		ifTrue: [ self optionAt: 'username' ]
		ifFalse: [ String new ].
	password := (self hasOption: 'password')
		ifTrue: [ self optionAt: 'password' ]
		ifFalse: [ String new ].
	^ Gofer new
		url: self repositoryUrl username: username password: password;
		yourself
]

{ #category : #accessing }
ConfigurationCommandLineHandler >> groups [
	^ (self optionAt: 'group') substrings: {$,}
]

{ #category : #actions }
ConfigurationCommandLineHandler >> handleMergeConflictDuring: aLoadBlock [
	[aLoadBlock 
	on: MCMergeOrLoadWarning do: [ :mergeConflict | mergeConflict merge ]]
	on: MCMergeResolutionRequest do: [ :request |  request autoMerge ].
]

{ #category : #testing }
ConfigurationCommandLineHandler >> hasConfiguration [
	self flag: 'TODO: should use proper cli parser Coral??'.
	^ (self arguments size > 1) and: [
		((self argumentAt: 2) beginsWith: '-') not ]
]

{ #category : #testing }
ConfigurationCommandLineHandler >> hasRepositoryUrl [
	^ self arguments size > 0 and: [ 
		| possibleUrl |
		possibleUrl := self arguments first.
		"check if the first argument is some sort of a URL"
		(possibleUrl includesSubstring: '://') or: [ possibleUrl first = $. ]]
]

{ #category : #actions }
ConfigurationCommandLineHandler >> installConfiguration [
	self installVersion: self configurationVersion
]

{ #category : #actions }
ConfigurationCommandLineHandler >> installVersion: aVersionName [
	| metacelloVersion |
	self inform: 'Installing ', self configurationName, ' ', aVersionName.
	
	metacelloVersion := (self metacelloVersion: aVersionName).
	
	self handleMergeConflictDuring: [
		(self hasOption: 'group')
			ifTrue: [ metacelloVersion load: self groups ]
			ifFalse: [ metacelloVersion load ]].
]

{ #category : #actions }
ConfigurationCommandLineHandler >> list [
	(self hasConfiguration or: [ self hasRepositoryUrl ])
		ifTrue: [ self listConfigurationDetails ]
		ifFalse: [ self listConfigurations ]
]

{ #category : #actions }
ConfigurationCommandLineHandler >> listConfigurationDetails [
	"List possible configurations of the given repository"
	
	self printMetacelloProjectDetails: self project.
	
	^ self exitSuccess
]

{ #category : #actions }
ConfigurationCommandLineHandler >> listConfigurations [
	"List possible configurations of the given repository"
	
	| configurations |
	
	self inform: 'All Configurations found in ', self repositoryUrl.
	
	configurations := self loadConfigurationNames.
	configurations ifEmpty: [ ^ self errorNoConfigurationsFound ].
	self printConfigurations: configurations.
	
	^ self exitSuccess
]

{ #category : #actions }
ConfigurationCommandLineHandler >> loadConfigurationNames [
	| references |
	
	references := self gofer allResolved.
		
	references := references 
			collect: #packageName 
			thenSelect: [ :name| name beginsWith: 'ConfigurationOf' ].
	
	^ references asSet asSortedCollection 
]

{ #category : #accessing }
ConfigurationCommandLineHandler >> loadRepositoryUrl [
	| possibleUrl |
	possibleUrl := self argumentAt: 1.
	(possibleUrl includesSubstring: '://')
		ifTrue: [ ^ possibleUrl asUrl ].
	^ possibleUrl asFileReference
]

{ #category : #actions }
ConfigurationCommandLineHandler >> metacelloVersion: aVersionName [
	| project |
	project := self project.
	^ [ project version: aVersionName ] 
		on: MetacelloVersionDoesNotExistError do: [ :error |
			aVersionName = 'last'
				ifTrue: [
					"manual fallback since there is no symbolic name for lastVersion"
					project lastVersion ]
				ifFalse: [ 
					"symbols and strings are not equal in Meteacello..."
					project version: aVersionName asSymbol ]].
]

{ #category : #accessing }
ConfigurationCommandLineHandler >> noQuit [
	^ self hasOption: 'no-quit'
]

{ #category : #accessing }
ConfigurationCommandLineHandler >> noSave [
	^ self hasOption: 'no-save'
]

{ #category : #actions }
ConfigurationCommandLineHandler >> postConfiguration [
	Smalltalk snapshot: (self noSave not) andQuit: (self noQuit not).

]

{ #category : #printing }
ConfigurationCommandLineHandler >> printConfigurations: configurations [

	self inform: (String streamContents:  [ :s|
		s
			nextPutAll: 'Found ';
			print: configurations size;
			nextPutAll: ' Configuration';
			nextPutAll: (configurations size = 1 ifTrue: [':'] ifFalse: ['s:']) ]).
	
	configurations do: [ :name | 
			self << name. 
			self stdout lf ].
	
]

{ #category : #printing }
ConfigurationCommandLineHandler >> printMetacelloProjectDetails: metacelloProject [
	| versions |
	self inform: 'Available versions for ', self configurationName.
	
	versions := metacelloProject symbolicVersionMap keys asSet.
	versions addAll: metacelloProject map keys.
	versions add: #last.
	
	"print the found version sorted alphabetically"
	versions asArray sorted do:[ :key|
		self stdout nextPutAll: key; lf ]
]

{ #category : #accessing }
ConfigurationCommandLineHandler >> project [
	^ self configuration project
]

{ #category : #accessing }
ConfigurationCommandLineHandler >> repositoryUrl [
	^ repositoryURL ifNil: [ 
		self hasRepositoryUrl 
			ifFalse: [ ^ self defaultRepositoryUrl ].
		repositoryURL := self loadRepositoryUrl ]
]
