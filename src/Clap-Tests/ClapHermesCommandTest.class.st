Class {
	#name : 'ClapHermesCommandTest',
	#superclass : 'ClapPharoCommandsTest',
	#category : 'Clap-Tests-Commands',
	#package : 'Clap-Tests',
	#tag : 'Commands'
}

{ #category : 'tests' }
ClapHermesCommandTest >> testDuplicationModeIsFailWhenOptionNotSpecified [
	| command |
	context := HermesClapCommand commandSpecification activationWith: #('loadHermes' 'foo.hermes').
	command := context command.
	
	self 
		assert: command duplicationMode
		equals: #fail
]

{ #category : 'tests' }
ClapHermesCommandTest >> testDuplicationModeIsFailWhenOptionSpecifiedAsFail [
	| command |
	context := HermesClapCommand commandSpecification activationWith: #('loadHermes' 'foo.hermes' '--on-duplication' 'fail' ).
	command := context command.
	
	self 
		assert: command duplicationMode
		equals: #fail
]

{ #category : 'tests' }
ClapHermesCommandTest >> testDuplicationModeIsIgnoreWhenOptionSpecifiedAsIgnore [
	| command |
	context := HermesClapCommand commandSpecification activationWith: #('loadHermes' 'foo.hermes' '--on-duplication' 'ignore' ).
	command := context command.
	
	self 
		assert: command duplicationMode
		equals: #ignore
]

{ #category : 'tests' }
ClapHermesCommandTest >> testGetManyFilesToProcessWhenManyHermesFileGivenInArguments [

	| command |
	context := HermesClapCommand commandSpecification activationWith:
		           #( 'loadHermes' 'foo.hermes' 'bar.hermes' 'baz.hermes' ).
	command := context command.

	self
		assertCollection: command allHermesFiles
		hasSameElements: #( 'foo.hermes' 'bar.hermes' 'baz.hermes' )
]

{ #category : 'tests' }
ClapHermesCommandTest >> testGetOneFileToProcessWheOneHermesFileGivenInArguments [

	| command |
	context := HermesClapCommand commandSpecification activationWith:
		           #( 'loadHermes' 'foo.hermes' ).
	command := context command.

	self
		assertCollection: command allHermesFiles
		hasSameElements: #( 'foo.hermes' )
]

{ #category : 'tests' }
ClapHermesCommandTest >> testNoFailOnUndeclaredOption [
	| command |
	context := HermesClapCommand commandSpecification activationWith: #('loadHermes' 'foo.hermes' '--no-fail-on-undeclared' ).
	command := context command.
	
	self deny: command shouldFailOnUndeclared
]

{ #category : 'tests' }
ClapHermesCommandTest >> testRunningHermesWithoutFileExitsWithErrorMessage [
	context := HermesClapCommand commandSpecification activateWith: #('loadHermes').
	
	self assertFailure.
	self
		assert: context exitException messageText
		equals: 'Missing Hermes file as argument'
]

{ #category : 'tests' }
ClapHermesCommandTest >> testRunningHermesWithoutHermesFileExitsWithErrorMessage [
	context := HermesClapCommand commandSpecification activateWith: #('loadHermes' 'foo.txt').
	
	self assertFailure.
	self
		assert: context exitException messageText
		equals: 'Missing Hermes file as argument'
]
