Class {
	#name : 'ClapApplicationTest',
	#superclass : 'TestCase',
	#instVars : [
		'context'
	],
	#category : 'Clap-Tests-Integration',
	#package : 'Clap-Tests',
	#tag : 'Integration'
}

{ #category : 'tests' }
ClapApplicationTest >> assertStdOutIncludes: aString [

	self 
		assert: (self stdoutString includesSubstring: aString)
		description: (aString surroundedBy: '"') , ' not found in ', (self stdoutString surroundedBy: '"')

]

{ #category : 'helpers' }
ClapApplicationTest >> stdoutString [

	^ context binaryStdout contents utf8Decoded
]

{ #category : 'tests' }
ClapApplicationTest >> testCanGetFlags [

	| command |
	context := ClapCommandLineExamples hello activateWith: #('hello' 'world' '--shout' '--language' 'fr').
	command := context command.

	self 
		assertCollection: command flags 
		hasSameElements: #('shout' 'language')
]

{ #category : 'tests' }
ClapApplicationTest >> testCanPrintHelp [

	context := ClapCommandLineExamples git activateWith: #().
	context command printHelp.
	
	self assertStdOutIncludes: 'Usage: git'.
	self assertStdOutIncludes: 'The stupid content tracker'.
]

{ #category : 'tests' }
ClapApplicationTest >> testCanPrintHelpWhenSubcommand [

	context := ClapCommandLineExamples git activateWith: #('git' 'add').
	context command printHelp.
	
	self assertStdOutIncludes: 'Usage: git add'.
	self assertStdOutIncludes: 'Add file contents to the index'
]
