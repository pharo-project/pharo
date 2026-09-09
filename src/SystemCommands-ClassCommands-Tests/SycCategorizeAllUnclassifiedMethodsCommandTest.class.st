"
A SycCategorizeAllUnclassifiedMethodsCommandTest is a test class for testing the behavior of SycCategorizeAllUnclassifiedMethodsCommand
"
Class {
	#name : 'SycCategorizeAllUnclassifiedMethodsCommandTest',
	#superclass : 'TestCase',
	#category : 'SystemCommands-ClassCommands-Tests-Tests',
	#package : 'SystemCommands-ClassCommands-Tests',
	#tag : 'Tests'
}

{ #category : 'running' }
SycCategorizeAllUnclassifiedMethodsCommandTest >> setUp [

	super setUp.

	SycCategorizeAllUnclassifiedTestClass methods do: [ :each |
		each unclassify ]
]

{ #category : 'running' }
SycCategorizeAllUnclassifiedMethodsCommandTest >> tearDown [

	SycCategorizeAllUnclassifiedTestClass methods do: [ :each |
		each unclassify ].

	super tearDown
]

{ #category : 'tests' }
SycCategorizeAllUnclassifiedMethodsCommandTest >> testHandlesEachMethodIndividually [

	| command |
	self
		assert:
		(SycCategorizeAllUnclassifiedTestClass >> #initialize) protocolName
		equals: Protocol unclassified.
	self
		assert:
		(SycCategorizeAllUnclassifiedTestClass >> #name) protocolName
		equals: Protocol unclassified.

	command := SycCategorizeAllUnclassifiedMethodsCommand for:
		           { SycCategorizeAllUnclassifiedTestClass }.

	command execute.

	self
		deny:
		(SycCategorizeAllUnclassifiedTestClass >> #initialize) protocolName
		equals:
		(SycCategorizeAllUnclassifiedTestClass >> #name) protocolName
]
