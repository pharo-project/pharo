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

{ #category : 'helpers' }
ClapApplicationTest >> assertStdOutIncludes: aString [

	self 
		assert: (self stdoutString includesSubstring: aString)
		description: (aString surroundedBy: '"') , ' not found in ', (self stdoutString surroundedBy: '"')

]

{ #category : 'tests - data' }
ClapApplicationTest >> specWithFlagWithMultiplePositionals [

	^ (ClapCommandSpec id: #test_command)
		addFlag: #shout description: 'shout flag';
		addPositional: #who description: 'who flag';
		addFlag: #language description: 'desc' positionalSpec: [ :positional |
			positional multiple: true ];
		yourself
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
ClapApplicationTest >> testCanGetMultiplePositional [

	| command |
	context := self specWithFlagWithMultiplePositionals activationWith: #('test_command' 'world' '--shout' '--language' 'fr' 'it').
	command := context command.

	self 
		assert: (command positional: #language) asArray
		equals: #('fr' 'it')
]

{ #category : 'tests' }
ClapApplicationTest >> testCanGetPositional [

	| command |
	context := ClapCommandLineExamples hello activateWith: #('hello' 'world' '--shout' '--language' 'fr').
	command := context command.

	self 
		assert: (command positional: #who) asArray
		equals: #('world')
]

{ #category : 'tests' }
ClapApplicationTest >> testCanGetPositionalWithFlag [

	| command |
	context := ClapCommandLineExamples hello activateWith: #('hello' 'world' '--shout' '--language' 'fr').
	command := context command.

	self 
		assert: (command positional: #language)
		equals: 'fr'
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
