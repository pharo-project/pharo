Class {
	#name : 'ClapContextTest',
	#superclass : 'TestCase',
	#instVars : [
		'context'
	],
	#category : 'Clap-Tests-Integration',
	#package : 'Clap-Tests',
	#tag : 'Integration'
}

{ #category : 'running' }
ClapContextTest >> setUp [
	super setUp.
	context := ClapContext specification: ClapCommandLineExamples hello
]

{ #category : 'tests' }
ClapContextTest >> testCanGetCommandInstanceRelatedToLastSubcommand [

	| command |
	
	context := ClapContext specification: ClapCommandLineExamples git.
	command := context 
		arguments: #( 'git' 'add');
		match;
		validateAll;
		command.

	self assert: command class equals: ClapGitAddCommandExample.
]

{ #category : 'tests' }
ClapContextTest >> testCanGetCommandSpecWhenNoSubcommand [

	| command |
	
	command := context 
		arguments: #();
		match;
		command.

	self assert: command class equals: context specification commandClass.
]

{ #category : 'tests' }
ClapContextTest >> testClapCommandDoesNotThrowError [
	"Ensure Root Clap Command mismatch does not raise an error but prints command help"
	
	context := ClapContext withPragmaCommands
		           arguments: #(  );
		           match;
		           yourself.

	self deny: context isMatch.
	self deny: context isValid.
	context executeToExit: [ :exitException | 
		"Command did not match, we expect an Exit was signaled"
		self assert:
			(context stdio stdout contents utf8Decoded includesSubstring:
				 'Entry point for commands implemented with Clap') ]
]

{ #category : 'tests' }
ClapContextTest >> testCommandMatchesWhenInvokedFromClapRoot [
	context := ClapContext withPragmaCommands
		arguments: #('image' 'cleanup');
		match;
		yourself.
	
	self assert: context isMatch.
	self assert: context isValid.
	self 
		assertCollection: context command commandSpecIdentifiers 
		hasSameElements: #('image' 'cleanup').
]

{ #category : 'tests' }
ClapContextTest >> testExecuteToExitPrintsValidationReportWhenValidationFails [
	"Ensure Root Clap Command mismatch does not raise an error but prints command help"
	
	context := ClapContext withPragmaCommands
		           arguments: #( 'hello' '--language' "missing lang value");
		           match;
		           yourself.

	self deny: context isValid.
	context executeToExit: [ :exitException | | errorString |
		"Command did not match, we expect an Exit was signaled"
		errorString := context stdio stderr contents utf8Decoded.
		self assert: (errorString includesSubstring: 'Usage: hello').
		self assert: (errorString includesSubstring: 'Missing positional value for language-value'). ]
]

{ #category : 'tests' }
ClapContextTest >> testShouldValidationBeKoWhenMismatched [
	| result |
	result := context
		arguments: #('hi' 'world');
		match.
	
	self assert: context isMismatch.
	self deny: context isValid.
	self assert: context leftovers notEmpty.
	self assert: result class equals: ClapMismatch.
]

{ #category : 'tests' }
ClapContextTest >> testShouldValidationBeOkWhenMatched [
	| result |
	result := context
		arguments: #('hello' 'world' '--help');
		match;
		command.
	
	self assert: context isMatch.
	self assert: context isValid.
	self assert: context leftovers isEmpty.
	self assert: result class equals: ClapCommandLineExamples.
]

{ #category : 'tests' }
ClapContextTest >> testShouldValidationFailWhenUnexpectedRootPositionalAfterSubcommand [
	context
		arguments: #('hello' 'world' '--directory' 'foo/bar/baz');
		match.
	
	self assert: context isMatch.
	self deny: context isValid.
	self
		assert: context leftovers
		equals: #('--directory' 'foo/bar/baz')
]

{ #category : 'tests' }
ClapContextTest >> testWrongFlagMatchingButInvalid [
	context
		arguments: #('hello' '--inexistent');
		match.
	
	self assert: context isMatch.
	self deny: context isValid.
	self
		assert: context leftovers
		equals: #('--inexistent')
]
