Class {
	#name : #ClapContextTest,
	#superclass : #TestCase,
	#instVars : [
		'context',
		'hello'
	],
	#category : #'Clap-Tests-Integration'
}

{ #category : #running }
ClapContextTest >> setUp [
	super setUp.
	hello := ClapCommandLineExamples hello.
	context := ClapContext specification: hello
]

{ #category : #running }
ClapContextTest >> testMatch [
	context
		arguments: #('hello' 'world');
		match.
	
	self assert: context isMatch.
	self assert: context isValid.
	self assert: context leftovers isEmpty
]

{ #category : #running }
ClapContextTest >> testRootPositionalAfterSubcommand [
	context
		arguments: #('hello' 'world' '--directory' 'foo/bar/baz');
		match.
	
	self assert: context isMatch.
	self deny: context isValid.
	self
		assert: context leftovers
		equals: #('--directory' 'foo/bar/baz')

]

{ #category : #running }
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
