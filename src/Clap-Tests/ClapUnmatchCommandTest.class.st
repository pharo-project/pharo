Class {
	#name : 'ClapUnmatchCommandTest',
	#superclass : 'TestCase',
	#instVars : [
		'context'
	],
	#category : 'Clap-Tests-Integration',
	#package : 'Clap-Tests',
	#tag : 'Integration'
}

{ #category : 'tests' }
ClapUnmatchCommandTest >> testCanGetAvailableCommands [

	context := ClapContext specification: ClapCommandLineExamples git.

	self
		should: [
				context
					arguments: #( 'foo' 'bar' );
					match;
					lastSubcommand ]
		raise: ClapUnmatchCommand
		withExceptionDo: [ :error | 
			self assert: error availableCommands equals: 'Available commands: helpCommand, add, branch, clone, commit, init, remote, status' ]
]

{ #category : 'tests' }
ClapUnmatchCommandTest >> testCanGetContextArguments [

	context := ClapContext specification: ClapCommandLineExamples git.

	self
		should: [
				context
					arguments: #( 'foo' 'bar' );
					match;
					lastSubcommand ]
		raise: ClapUnmatchCommand
		withExceptionDo: [ :error | self assert: error contextArguments equals: 'Context arguments: #(''foo'' ''bar'')' ]
]
