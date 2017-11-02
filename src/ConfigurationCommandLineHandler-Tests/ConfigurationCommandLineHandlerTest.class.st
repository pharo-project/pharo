"
SUnit tests for class ConfigurationCommandLineHandler
"
Class {
	#name : #ConfigurationCommandLineHandlerTest,
	#superclass : #TestCase,
	#category : 'ConfigurationCommandLineHandler-Tests'
}

{ #category : #convenience }
ConfigurationCommandLineHandlerTest >> activate: arguments [
	^ PharoCommandLineHandler activateWith: (self argumentsWith: arguments)
]

{ #category : #convenience }
ConfigurationCommandLineHandlerTest >> argumentsWith: aCollection [
	^ CommandLineArguments withArguments: aCollection
]

{ #category : #convenience }
ConfigurationCommandLineHandlerTest >> command: arguments [
	^ ConfigurationCommandLineHandler commandLine: (self argumentsWith: arguments)
]

{ #category : #tests }
ConfigurationCommandLineHandlerTest >> testGroups [
	| cli |
	cli := self command: #('http://ss3.gemstone.com/ss/MetaRepoForPharo20' '--group=foo').
	self assertCollection: cli groups equals: {'foo'}.
	
	cli := self command: #('http://ss3.gemstone.com/ss/MetaRepoForPharo20' '--group=foo,boo').
	self assertCollection: cli groups equals: {'foo' . 'boo'}.
]

{ #category : #tests }
ConfigurationCommandLineHandlerTest >> testHasConfiguration [
	| cli |
	cli := self command: #('http://ss3.gemstone.com/ss/MetaRepoForPharo20').
	self deny: cli hasConfiguration.
	
	cli := self command: #('http://ss3.gemstone.com/ss/MetaRepoForPharo20' '--install').
	self deny: cli hasConfiguration.
	
	cli := self command: #('http://ss3.gemstone.com/ss/MetaRepoForPharo20' 'ConfigurationOfFoo').
	self assert: cli hasConfiguration.
]

{ #category : #tests }
ConfigurationCommandLineHandlerTest >> testListConfigurations [
	self skip: 'Currently quits the image when in non-interactive mode'.
	self activate: #('config' 'http://ss3.gemstone.com/ss/MetaRepoForPharo20').
]

{ #category : #tests }
ConfigurationCommandLineHandlerTest >> testResponsibility [
	| args |
	args := self argumentsWith: #('config').
	self assert: (ConfigurationCommandLineHandler isResponsibleFor: args).
]
