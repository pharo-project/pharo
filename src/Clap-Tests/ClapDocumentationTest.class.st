Class {
	#name : 'ClapDocumentationTest',
	#superclass : 'TestCase',
	#category : 'Clap-Tests-Unit',
	#package : 'Clap-Tests',
	#tag : 'Unit'
}

{ #category : 'tests' }
ClapDocumentationTest >> testFlagWithPositionalShouldPrintBothFlagAndPositional [
	| command doc lines |
	command := (ClapCommandSpec id: #testCommand)
		description: 'Test command for flags with positional';
		add: ClapFlag forHelp;
		add:
			((ClapFlag id: #language)
				description: 'Select language of greeting';
				add: ((ClapPositional id: #langCode)
					meaning: [ :pos | pos word asSymbol ];
					implicitMeaning: [ :arg :app | app defaultLanguage ]);
				yourself);
		yourself.
	
	doc := ClapDocumenter stringFrom: [ :documenter | documenter explain: command ].
	lines := doc lines.
	
	self assert: (lines third includesSubstring: '[--language <langCode>]').
	self assert: (lines seventh includesSubstring: '--language <langCode>').

]

{ #category : 'tests' }
ClapDocumentationTest >> testHelloShortUsage [
	| command doc lines |
	command := ClapCommandLineExamples hello.
	
	doc := ClapDocumenter stringFrom: [ :documenter | documenter explain: command ].
	lines := doc lines.
	
	self assert: (doc endsWith: OSPlatform current lineEnding).
	self assert: lines first equals: 'Provides greetings'.
	self assert: lines second isEmpty.
	self assert: (lines third beginsWith: 'Usage: hello').
	self assert: (lines third includesSubstring: '[--language <language-value>]').
	self assert: (lines third endsWith: '[<who>]').
]
