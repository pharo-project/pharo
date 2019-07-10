Class {
	#name : #ClapDocumentationTest,
	#superclass : #TestCase,
	#category : #'Clap-Tests-Integration'
}

{ #category : #tests }
ClapDocumentationTest >> testHelloShortUsage [
	| command doc lines |
	command := ClapCommandLineExamples hello.
	
	doc := ClapDocumenter stringFrom: [ :documenter | documenter explain: command ].
	lines := doc lines.
	
	self assert: (doc endsWith: OSPlatform current lineEnding).
	self assert: lines first equals: 'Provides greetings'.
	self assertEmpty: lines second.
	self assert: (lines third beginsWith: 'Usage: hello').
	self assert: (lines third endsWith: '[<who>]').

]
