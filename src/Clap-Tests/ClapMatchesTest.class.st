"
Tests around matching non-trivial commands and parameters.

In the absence of a parent context however, we can't test invalid or extra arguments; see ClapContextTest for this.
"
Class {
	#name : 'ClapMatchesTest',
	#superclass : 'TestCase',
	#instVars : [
		'clap',
		'dirFlag',
		'slowFlag'
	],
	#category : 'Clap-Tests-Integration',
	#package : 'Clap-Tests',
	#tag : 'Integration'
}

{ #category : 'running' }
ClapMatchesTest >> setUp [
	super setUp.
	dirFlag := (ClapFlag id: #directory)
		add: (ClapPositional id: #DIR);
		yourself.
	slowFlag := ClapFlag id: #slow.
	
	clap := (ClapCommandSpec id: #clap)
		add: dirFlag;
		add: ((ClapCommandSpec id: #this)
			add: slowFlag; yourself);
			yourself;
		add: ((ClapCommandSpec id: #that)
			add: (ClapFlag id: #encore);
			add: (ClapPositional id: #clappee);
			yourself);
		yourself
]

{ #category : 'tests' }
ClapMatchesTest >> testFlagAbsentValue [
	| args argv match flagMatch |
	slowFlag meaning: [ :flag | flag isMismatch not ].
	args := #('clap' 'this').
	argv := args readStream.
	
	match := clap matchOn: argv readStream in: (clap activationWith: args).
	flagMatch := (match at: #this) at: slowFlag.

	self deny: flagMatch isExplicit.
	self
		assert: flagMatch parent parent
		identicalTo: match.
	self assert: flagMatch value equals: nil

]

{ #category : 'tests' }
ClapMatchesTest >> testFlagOmittedValue [
	| match flagMatch |
	clap add: slowFlag. "we need another flag to follow the incomplete one"
	
	match := clap match: #('clap' '--directory' '--slow').
	flagMatch := match at: dirFlag.

	self assert: flagMatch isExplicit.
	self assert: flagMatch value equals: nil.
	self deny: (flagMatch at: #DIR) isExplicit.
	self assert: (match at: slowFlag) isExplicit.
]

{ #category : 'tests' }
ClapMatchesTest >> testFlagValue [
	| match flagMatch |
	slowFlag meaning: [ :flag | flag isMismatch not ].
	
	match := clap match: #('clap' 'this' '--slow').
	flagMatch := ((match at: #this) at: slowFlag).
	
	self assert: flagMatch isExplicit.
	self
		assert: flagMatch parent parent
		identicalTo: match.
	self assert: flagMatch value
]

{ #category : 'tests' }
ClapMatchesTest >> testMultiplePositional [
	| match |
	clap add: ((ClapPositional id: #list) multiple: true).
	
	match := clap match: #('clap' 'foo' 'bar' 'baz' '-d' 'a/b/c').
	
	self deny: match isMismatch.
	self assert: (match includesMatchOf: #directory).
	self assert: (match includesMatchOf: #list).
	self
		assertCollection: (match occurrencesOf: #list collect: #value) asArray
		equals: #('foo' 'bar' 'baz').
	self
		assertCollection: ((match at: #list) allOccurrencesCollect: #value) asArray
		equals: #('foo' 'bar' 'baz').
]

{ #category : 'tests' }
ClapMatchesTest >> testParameterizedFlagValue [
	| match |
	dirFlag meaning: [ :flag | Path from: (flag at: #DIR) value ].
	
	match := clap match: #('clap' '--directory' 'foo/bar/baz').
	
	self
		assert: (match at: dirFlag) value
		equals: (Path * 'foo' / 'bar' / 'baz').
]

{ #category : 'tests' }
ClapMatchesTest >> testRootPositionalBeforeSubcommand [
	| match |
	match := clap match: #('clap' '--directory' 'foo/bar/baz' 'this').
	
	self deny: match isMismatch.
	self assert: (match includesMatchOf: #directory).
	self assert: (match includesMatchOf: #this).
	self deny: (match includesMatchOf: #that).
	self
		assert: ((match at: #directory) at: #DIR) value
		equals: 'foo/bar/baz'.
	self
		assert: (match at: dirFlag) positionalValues asArray
		equals: #( 'foo/bar/baz' )
]

{ #category : 'tests' }
ClapMatchesTest >> testSubcommandAccessing [
	| match |
	match := clap match: #('clap' 'this').
	
	self deny: match isMismatch.
	self deny: (match includesMatchOf: #directory).
	self assert: (match includesMatchOf: #this).
	self deny: (match includesMatchOf: #that).
	self deny: (match at: #this) isMismatch.
	self deny: ((match at: #this) includesMatchOf: #slow).
	
	self
		should: [ match at: #this ifPresent: [ Notification signal ] ]
		raise: Notification.
	self
		should: [ match at: #this ifPresent: [ Notification signal ] ifAbsent: [ self fail ] ]
		raise: Notification.
]

{ #category : 'tests' }
ClapMatchesTest >> testThis [
	| match |
	match := clap match: #('clap' 'this').
	
	self deny: match isMismatch.
	self deny: (match includesMatchOf: #directory).
	self assert: (match includesMatchOf: #this).
	self deny: (match includesMatchOf: #that).
	self deny: (match at: #this) isMismatch.
	self deny: ((match at: #this) includesMatchOf: #slow)
]

{ #category : 'tests' }
ClapMatchesTest >> testWrongCommandMismatches [
	| match |
	match := clap match: #('foo' '-d' 'bar/baz/qux' 'this').
	
	self assert: match isMismatch.
	self
		assert: match word
		equals: 'foo'
]
