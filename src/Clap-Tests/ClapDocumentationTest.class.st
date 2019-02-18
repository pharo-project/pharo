Class {
	#name : #ClapDocumentationTest,
	#superclass : #TestCase,
	#category : #'Clap-Tests-Integration'
}

{ #category : #tests }
ClapDocumentationTest >> testHelloShortUsage [
	| command doc |
	command := ClapCommandLineExamples hello.
	
	doc := ClapDocumenter stringFrom: [ :documenter | documenter explain: command ].
	
	self assert: (doc endsWith: OSPlatform current lineEnding).
	self
		assert: (doc lines first: 3)
		equals: #(
			'Provides greetings'
			''
			'Usage: hello [--help] [--whisper] [--shout] [<who>]'
		)
]
