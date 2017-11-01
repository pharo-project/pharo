"
A MetacelloCommandLineHandlerTest is a test class for testing the behavior of MetacelloCommandLineHandler
"
Class {
	#name : #MetacelloCommandLineHandlerTest,
	#superclass : #TestCase,
	#category : 'MetacelloCommandLineHandler-Tests'
}

{ #category : #convenience }
MetacelloCommandLineHandlerTest >> argumentsWith: aCollection [
	^ CommandLineArguments withArguments: aCollection
]

{ #category : #convenience }
MetacelloCommandLineHandlerTest >> command: arguments [
	^ MetacelloCommandLineHandler commandLine: (self argumentsWith: arguments)
]

{ #category : #tests-accessing }
MetacelloCommandLineHandlerTest >> testBaselineOrConfigurationName [
	| cli |
	cli := self command: #('install' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper').
	self assert: cli baselineOrConfigurationName equals: 'BaselineOfMineSweeper'.
	
	cli := self command: #('install' 'http://ss3.gemstone.com/ss/MetaRepoForPharo20' 'ConfigurationOfFoo').
	self assert: cli baselineOrConfigurationName equals: 'ConfigurationOfFoo'.
]

{ #category : #tests-accessing }
MetacelloCommandLineHandlerTest >> testBaselineOrConfigurationNameSuffix [
	| cli |
	cli := self command: #('install' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper').
	self assert: cli baselineOrConfigurationNameSuffix equals: 'MineSweeper'.
	
	cli := self command: #('install' 'http://ss3.gemstone.com/ss/MetaRepoForPharo20' 'ConfigurationOfFoo').
	self assert: cli baselineOrConfigurationNameSuffix equals: 'Foo'.
]

{ #category : #tests-activation }
MetacelloCommandLineHandlerTest >> testCheckInputForInstallSubcommand [
	| cli |
	cli := self command: #('install' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper' '--version=master').
	self should: [ cli checkInputForInstallSubcommand ] raise: Exit.
	
	cli := self command: #('install' 'http://ss3.gemstone.com/ss/MetaRepoForPharo20' 'Foo').
	self should: [ cli checkInputForInstallSubcommand ] raise: Exit.
]

{ #category : #tests-accessing }
MetacelloCommandLineHandlerTest >> testGroups [
	| cli |
	cli := self command: #('install' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper' '--groups=core,tests').
	self assertCollection: cli groups hasSameElements: #('core' 'tests')
]

{ #category : #tests-testing }
MetacelloCommandLineHandlerTest >> testHasGroups [
	| cli |
	cli := self command: #('install' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper' '--groups=core,tests').
	self assert: cli hasGroups.
	
	cli := self command: #('install' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper').
	self deny: cli hasGroups.
]

{ #category : #tests-testing }
MetacelloCommandLineHandlerTest >> testHasVersion [
	| cli |
	cli := self command: #('install' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper' '--version=stable').
	self assert: cli hasVersion.
	
	cli := self command: #('install' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper').
	self deny: cli hasVersion.
]

{ #category : #tests-testing }
MetacelloCommandLineHandlerTest >> testIsBaselineName [
	| cli |
	cli := self command: #('install' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper').
	self assert: cli isBaselineName.
	
	cli := self command: #('install' 'http://ss3.gemstone.com/ss/MetaRepoForPharo20' 'ConfigurationOfFoo').
	self deny: cli isBaselineName.
]

{ #category : #tests-testing }
MetacelloCommandLineHandlerTest >> testIsConfigurationName [
	| cli |
	cli := self command: #('install' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper').
	self deny: cli isConfigurationName.
	
	cli := self command: #('install' 'http://ss3.gemstone.com/ss/MetaRepoForPharo20' 'ConfigurationOfFoo').
	self assert: cli isConfigurationName.
]

{ #category : #tests-accessing }
MetacelloCommandLineHandlerTest >> testRepositoryUrl [
	| cli |
	cli := self command: #('install' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper').
	self assert: cli repositoryUrl equals: 'github://juliendelplanque/MineSweeper/repository'.
]

{ #category : #tests-testing }
MetacelloCommandLineHandlerTest >> testShouldQuit [
	| cli |
	cli := self command: #('install' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper').
	self
		deny: cli noQuit;
		assert: cli shouldQuit.
	
	cli := self command: #('install' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper' '--no-quit').
	self
		assert: cli noQuit;
		deny: cli shouldQuit
]

{ #category : #tests-testing }
MetacelloCommandLineHandlerTest >> testShouldSave [
	| cli |
	cli := self command: #('install' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper').
	self
		deny: cli noSave;
		assert: cli shouldSave.
		
	cli := self command: #('install' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper' '--no-save').
	self
		assert: cli noSave;
		deny: cli shouldSave
]

{ #category : #tests-accessing }
MetacelloCommandLineHandlerTest >> testSubCommand [
	| cli |
	cli := self command: #('install' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper').
	self assert: cli subCommand equals: 'install'.
	
	cli := self command: #('foo' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper').
	self should: [ cli subCommand ] raise: Exit
]

{ #category : #tests-accessing }
MetacelloCommandLineHandlerTest >> testVersion [
	| cli |
	cli := self command: #('install' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper' '--version=stable').
	self assert: cli version equals: 'stable'
]
