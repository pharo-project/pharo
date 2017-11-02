"
Command line handler for dealing with Metacello from the command line.

Usage:
metacello --help
metacello install <repository url> (<baseline>|<configuration>) [--version=<version>] [--groups=<group name>,...] [--no-quit] [--no-save]
	<repository url>    A Monticello repository name 
	<baseline>          A valid Metacello Baseline name
	<configuration>     A valid Metacello Configuration name
	<version>           A valid version for the given Configuration (incompatible with Baseline)
	<group name>        A valid Metacello group name(s)
	--help              Show this help message
	--no-quit           Keep the image running after Baseline/Configuration install
	--no-save           Don't save the image after Baseline/Configuration install
	
Examples:
	# Display this help message
	pharo Pharo.image metacello OR pharo Pharo.image metacello --help
	
	# Install group 'Core' and 'Tests' of latest version
	pharo Pharo.image metacello install $REPOS_URL BaselineOfFoo --groups=Core,Tests
	
	# Install a specific version '1.5' and only a specific group 'Tests'
	pharo Pharo.image metacello install $REPOS_URL ConfigurationOfFoo --version=1.5 --groups=Tests
	
	
"
Class {
	#name : #MetacelloCommandLineHandler,
	#superclass : #CommandLineHandler,
	#category : 'MetacelloCommandLineHandler-Core'
}

{ #category : #accessing }
MetacelloCommandLineHandler class >> commandName [
	^ 'metacello'
]

{ #category : #accessing }
MetacelloCommandLineHandler class >> description [
	^ 'Install and inspect Metacello Baselines/Configurations from the command line'
]

{ #category : #activation }
MetacelloCommandLineHandler >> activate [
	self activateHelpWithoutArguments
		ifTrue: [ ^ self ].
	self subCommand = 'install'
		ifTrue: [ 
			self
				checkInputForInstallSubcommand;
				install;
				postInstall ]
]

{ #category : #accessing }
MetacelloCommandLineHandler >> availableSubCommands [
	^ #('install')
]

{ #category : #accessing }
MetacelloCommandLineHandler >> baselineOrConfigurationName [
	^ self argumentAt: 3
]

{ #category : #accessing }
MetacelloCommandLineHandler >> baselineOrConfigurationNameSuffix [
	self isBaselineName
		ifTrue: [ ^ self baselineOrConfigurationName copyFrom: 'BaselineOf' size + 1 to: self baselineOrConfigurationName size ].
		
	self isConfigurationName
		ifTrue: [ ^ self baselineOrConfigurationName copyFrom: 'ConfigurationOf' size + 1 to: self baselineOrConfigurationName size ]
]

{ #category : #activation }
MetacelloCommandLineHandler >> checkInputForInstallSubcommand [
	(self isBaselineName and: [ self hasVersion ])
		ifTrue: [ Exit signalFailure: 'A baseline can not be used with --version option.' ].
	
	(self isBaselineName or: [ self isConfigurationName ])
		ifFalse: [ Exit signalFailure: 'Not a Baseline nor a Configuration.' ]
]

{ #category : #'argument validation' }
MetacelloCommandLineHandler >> checkSubCommand: aString [ 
	(self availableSubCommands includes: aString)
		ifFalse: [
			self exitFailure: ('{1} subcommand does not exists' format: { aString }) ]
]

{ #category : #accessing }
MetacelloCommandLineHandler >> groups [
	^ (self optionAt: 'groups') splitOn: $,
]

{ #category : #testing }
MetacelloCommandLineHandler >> hasGroups [
	^ self hasOption: 'groups'
]

{ #category : #testing }
MetacelloCommandLineHandler >> hasVersion [
	^ self hasOption: 'version'
]

{ #category : #activation }
MetacelloCommandLineHandler >> install [
	| metacello |
	metacello := Metacello new.
	self isBaselineName
		ifTrue: [ metacello baseline: self baselineOrConfigurationNameSuffix ].
	self isConfigurationName
		ifTrue: [ metacello configuration: self baselineOrConfigurationNameSuffix ].
	metacello repository: self repositoryUrl.
	self hasVersion
		ifTrue: [ metacello version: self version ].
	self hasGroups
		ifTrue: [ metacello load: self groups ]
		ifFalse: [ metacello load ]
]

{ #category : #testing }
MetacelloCommandLineHandler >> isBaselineName [
	^ self baselineOrConfigurationName matchesRegex: 'BaselineOf.+'
]

{ #category : #testing }
MetacelloCommandLineHandler >> isConfigurationName [
	^ self baselineOrConfigurationName matchesRegex: 'ConfigurationOf.+'
]

{ #category : #accessing }
MetacelloCommandLineHandler >> noQuit [
	^ self hasOption: 'no-quit'
]

{ #category : #accessing }
MetacelloCommandLineHandler >> noSave [
	^ self hasOption: 'no-save'
]

{ #category : #activation }
MetacelloCommandLineHandler >> postInstall [
	Smalltalk snapshot: self shouldSave andQuit: self shouldQuit
]

{ #category : #accessing }
MetacelloCommandLineHandler >> repositoryUrl [
	^ self argumentAt: 2
]

{ #category : #testing }
MetacelloCommandLineHandler >> shouldQuit [
	^ self noQuit not
]

{ #category : #testing }
MetacelloCommandLineHandler >> shouldSave [
	^ self noSave not
]

{ #category : #accessing }
MetacelloCommandLineHandler >> subCommand [
	| firstArg |
	firstArg := self argumentAt: 1.
	self checkSubCommand: firstArg.
	^ firstArg
]

{ #category : #accessing }
MetacelloCommandLineHandler >> version [
	^ self optionAt: 'version'
]
