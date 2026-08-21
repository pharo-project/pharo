Class {
	#name : 'HermesClapCommandTest',
	#superclass : 'ClapPharoCommandsTest',
	#category : 'Hermes-Extensions-Tests-Commands',
	#package : 'Hermes-Extensions-Tests',
	#tag : 'Commands'
}

{ #category : 'tests - duplication mode' }
HermesClapCommandTest >> testDuplicationModeIsFailWhenOptionNotSpecified [
	| command |
	context := HermesClapCommand commandSpecification activationWith: #('loadHermes' 'foo.hermes').
	command := context command.
	
	self 
		assert: command duplicationMode
		equals: #fail
]

{ #category : 'tests - duplication mode' }
HermesClapCommandTest >> testDuplicationModeIsFailWhenOptionSpecifiedAsFail [
	| command |
	context := HermesClapCommand commandSpecification activationWith: #('loadHermes' 'foo.hermes' '--on-duplication' 'fail' ).
	command := context command.
	
	self 
		assert: command duplicationMode
		equals: #fail
]

{ #category : 'tests - duplication mode' }
HermesClapCommandTest >> testDuplicationModeIsIgnoreWhenOptionSpecifiedAsIgnore [
	| command |
	context := HermesClapCommand commandSpecification activationWith: #('loadHermes' 'foo.hermes' '--on-duplication' 'ignore' ).
	command := context command.
	
	self 
		assert: command duplicationMode
		equals: #ignore
]

{ #category : 'tests - duplication mode' }
HermesClapCommandTest >> testDuplicationModeIsReplaceWhenOptionSpecifiedAsReplace [
	| command |
	context := HermesClapCommand commandSpecification activationWith: #('loadHermes' 'foo.hermes' '--on-duplication' 'replace' ).
	command := context command.
	
	self 
		assert: command duplicationMode
		equals: #replace
]

{ #category : 'tests - fail on undeclared' }
HermesClapCommandTest >> testFailOnUndeclaredWhenNoOption [
	| command |
	context := HermesClapCommand commandSpecification activationWith: #('loadHermes' 'foo.hermes').
	command := context command.
	
	self assert: command shouldFailOnUndeclared
]

{ #category : 'tests' }
HermesClapCommandTest >> testGetManyFilesToProcessWhenManyHermesFileGivenInArguments [

	| command |
	context := HermesClapCommand commandSpecification activationWith:
		           #( 'loadHermes' 'foo.hermes' 'bar.hermes' 'baz.hermes' ).
	command := context command.

	self
		assertCollection: command allHermesFiles
		hasSameElements: #( 'foo.hermes' 'bar.hermes' 'baz.hermes' )
]

{ #category : 'tests' }
HermesClapCommandTest >> testGetOneFileToProcessWheOneHermesFileGivenInArguments [

	| command |
	context := HermesClapCommand commandSpecification activationWith:
		           #( 'loadHermes' 'foo.hermes' ).
	command := context command.

	self
		assertCollection: command allHermesFiles
		hasSameElements: #( 'foo.hermes' )
]

{ #category : 'tests - fail on undeclared' }
HermesClapCommandTest >> testNoFailOnUndeclaredWhenOptionSpecified [
	| command |
	context := HermesClapCommand commandSpecification activationWith: #('loadHermes' 'foo.hermes' '--no-fail-on-undeclared' ).
	command := context command.
	
	self deny: command shouldFailOnUndeclared
]

{ #category : 'tests' }
HermesClapCommandTest >> testRunningHermesWithoutFileExitsWithErrorMessage [
	context := HermesClapCommand commandSpecification activateWith: #('loadHermes').
	
	self assertFailure.
	self
		assert: context exitException messageText
		equals: 'Missing Hermes file as argument'
]

{ #category : 'tests' }
HermesClapCommandTest >> testRunningHermesWithoutHermesFileExitsWithErrorMessage [
	context := HermesClapCommand commandSpecification activateWith: #('loadHermes' 'foo.txt').
	
	self assertFailure.
	self
		assert: context exitException messageText
		equals: 'Missing Hermes file as argument'
]
