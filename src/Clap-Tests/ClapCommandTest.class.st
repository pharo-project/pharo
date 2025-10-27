Class {
	#name : 'ClapCommandTest',
	#superclass : 'TestCase',
	#instVars : [
		'context'
	],
	#category : 'Clap-Tests-Integration',
	#package : 'Clap-Tests',
	#tag : 'Integration'
}

{ #category : 'tests' }
ClapCommandTest >> testShouldArgumentsBeOkWhenMatched [
	| command |
	context := ClapCommandLineExamples hello activateWith: #('hello' 'world' '--help').
	command := context command.
	
	self assertCollection: command commandSpecIdentifiers hasSameElements: #('hello').
	self assert: (command hasFlag: 'help').
	self assertCollection: (command positional: #who) hasSameElements: #('world').
]

{ #category : 'tests' }
ClapCommandTest >> testShouldGetClapApplicationWhenAskingCommandAndMismatch [

	| spec command |
	spec := ClapCommandSpec id: #hello.
	context := spec activationWith: #( 'hi' '--foo' '--bar' ).

	self assert: context match isMismatch.
	
	command := context command.
	
	self assert: command class equals: ClapApplication
]

{ #category : 'tests' }
ClapCommandTest >> testShouldGetCollectionWhenAskingForAMultiplePositional [
	| command |
	context := ClapContext specification: ClapCommandLineExamples hello.
	command := context
		arguments: #('hello' 'foo' 'bar');
		command.
	
	self 
		assertCollection: (command positional: #who)
		hasSameElements: #('foo' 'bar') 

]

{ #category : 'tests' }
ClapCommandTest >> testShouldGetCollectionWhenAskingForAMultiplePositionalWithOneValue [
	| command |
	context := ClapContext specification: ClapCommandLineExamples hello.
	command := context
		arguments: #('hello' 'foo');
		command.
	
	self 
		assertCollection: (command positional: #who)
		hasSameElements: #('foo') 

]

{ #category : 'tests' }
ClapCommandTest >> testShouldGetDefaultValueWhenAskingForAMissingNestedPositionalWithDefaultValueSpecified [
	| command |
	context := ClapContext specification: ClapCommandLineExamples hello.
	command := context
		arguments: #('hello');
		command.
	
	self 
		assert: (command positional: #language)
		equals: #en

]

{ #category : 'tests' }
ClapCommandTest >> testShouldGetDefaultValueWhenAskingForAMissingNestedPositionalWithDefaultValueSpecifiedOnSubcommand [
	| command spec |
	spec := (ClapCommandSpec id: #hello)
		add: ((ClapCommandSpec id: 'world')
			commandClass: ClapGitAddCommandExample;
			addFlag: #language 
				description: 'Select language of greeting' 
				positionalSpec: [ :positional |
					positional defaultValue: [ :arg :app | 'fr' ] ];
			yourself);
		yourself.
	context := ClapContext specification: spec.
	command := context
		arguments: #('hello' 'world');
		command.
	
	self 
		assert: (command positional: #language)
		equals: #fr
]

{ #category : 'tests' }
ClapCommandTest >> testShouldGetErrorWhenAskingLastSubcommandAndMismatch [

	| spec command |
	spec := ClapCommandSpec id: #hello.
	context := spec activationWith: #( 'hi' '--foo' '--bar' ).

	self assert: context match isMismatch.
	self 
		should: [ command := context lastSubcommand ] 
		raise: ClapUnmatchCommand
		withExceptionDo: [ :error | 
			self assert: (error description includesSubstring: 'foo') description: 'missing context arguments'.
			self assert: (error description includesSubstring: 'bar') description: 'missing availabel commands'.
			]
]

{ #category : 'tests' }
ClapCommandTest >> testShouldGetFlagPositionalValueWhenAskingForAFlagHavingPositional [
	| command |
	context := ClapContext specification: ClapCommandLineExamples hello.
	command := context
		arguments: #('hello' '--language' 'fr');
		command.
	
	self 
		assert: (command positional: #language)
		equals: #fr

]

{ #category : 'tests' }
ClapCommandTest >> testShouldReturnFalseWhenAskingForAMissingFlag [
	| command |
	context := ClapContext specification: ClapCommandLineExamples hello.
	command := context
		arguments: #('hello' 'world');
		command.
	
	self deny: (command hasFlag: '--help')
]

{ #category : 'tests' }
ClapCommandTest >> testShouldSubCommandsBeOkWhenMatched [
	| command |
	context := ClapContext specification: ClapCommandLineExamples git.
	command := context
		arguments: #('git' 'status');
		command.
	
	self 
		assertCollection: command commandSpecIdentifiers 
		hasSameElements: #('git' 'status').
]

{ #category : 'tests' }
ClapCommandTest >> testShouldThrowNotFoundWhenAskingForAMissingPositional [
	| spec command |
	
	spec := ClapCommandLineExamples hello.
	spec addFlagWithPositional: #foo description: 'some foo'.
	context := ClapContext specification: spec.
	command := context
		arguments: #('hello');
		command.
	
	self 
		should: [ command positional: #foo ]
		raise: NotFound

]

{ #category : 'tests' }
ClapCommandTest >> testShouldValidationFailWhenAskingForAPositionalWithMissingValues [
	| spec match validationReport problem |
	
	spec := ClapCommandSpec id: #hello.
	spec addFlagWithPositional: #foo description: 'some foo'.
	spec addFlagWithPositional: #bar description: 'some bar'.
	context := ClapContext specification: spec.
	match := context
		arguments: #('hello' '--foo' '--bar');
		match.
	validationReport := context allValidations.
	
	self deny: match isMismatch.
	self deny: validationReport isSuccess.
	problem := validationReport problems first validation.
	self assert: problem class equals: ClapNestedPositionalValidation.
	self 
		assertCollection: (validationReport problems flatCollect: [:pb | pb validation missingValueIdentifiers])
		hasSameElements: #('foo-value' 'bar-value').
	
]
