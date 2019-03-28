"
Tests around matching non-trivial commands and parameters.

In the absence of a parent context however, we can't test invalid or extra arguments; see ClapContextTest for this.
"
Class {
	#name : #ClapMatchesTest,
	#superclass : #TestCase,
	#instVars : [
		'clap',
		'dirFlag',
		'slowFlag'
	],
	#category : #'Clap-Tests-Integration'
}

{ #category : #running }
ClapMatchesTest >> setUp [
	super setUp.
	dirFlag := (ClapFlag withName: 'directory')
		add: (ClapPositional withName: 'DIR');
		yourself.
	slowFlag := ClapFlag withName: 'slow'.
	
	clap := (ClapCommand withName: 'clap')
		add: dirFlag;
		add: ((ClapCommand withName: 'this')
			add: slowFlag; yourself);
			yourself;
		add: ((ClapCommand withName: 'that')
			add: (ClapFlag withName: 'encore');
			add: (ClapPositional withName: 'clappee');
			yourself);
		yourself
]

{ #category : #running }
ClapMatchesTest >> testFlagAbsentValue [
	| match flagMatch |
	slowFlag meaning: [ :flag | flag isMismatch not ].
	
	match := clap matchOn: #('clap' 'this') readStream.
	flagMatch := (match atName: 'this') at: slowFlag.

	self deny: flagMatch isExplicit.
	self
		assert: flagMatch parent parent
		identicalTo: match.
	self deny: flagMatch value

]

{ #category : #running }
ClapMatchesTest >> testFlagOmittedValue [
	| match flagMatch |
	clap add: slowFlag. "we need another flag to follow the incomplete one"
	
	match := clap match: #('clap' '--directory' '--slow').
	flagMatch := match at: dirFlag.

	self assert: flagMatch isExplicit.
	self
		deny: flagMatch value
		identicalTo: '--slow'.
	self deny: (flagMatch atName: 'DIR') isExplicit.
	self assert: (match at: slowFlag) isExplicit.

]

{ #category : #running }
ClapMatchesTest >> testFlagValue [
	| match flagMatch |
	slowFlag meaning: [ :flag | flag isMismatch not ].
	
	match := clap match: #('clap' 'this' '--slow').
	flagMatch := ((match atName: 'this') at: slowFlag).
	
	self assert: flagMatch isExplicit.
	self
		assert: flagMatch parent parent
		identicalTo: match.
	self assert: flagMatch value

]

{ #category : #running }
ClapMatchesTest >> testMultiplePositional [
	| match |
	clap add: ((ClapPositional withName: 'list') multiple: true).
	
	match := clap match: #('clap' 'foo' 'bar' 'baz' '-d' 'a/b/c').
	
	self deny: match isMismatch.
	self assert: (match includesMatchNamed: 'directory').
	self assert: (match includesMatchNamed: 'list').
	self
		assertCollection: (match occurrencesNamed: 'list' collect: #value) asArray
		equals: #('foo' 'bar' 'baz').
	self
		assertCollection: ((match atName: 'list') allOccurrencesCollect: #value) asArray
		equals: #('foo' 'bar' 'baz').
]

{ #category : #running }
ClapMatchesTest >> testParameterizedFlagValue [
	| match |
	dirFlag meaning: [ :flag | Path from: (flag atName: 'DIR') value ].
	
	match := clap match: #('clap' '--directory' 'foo/bar/baz').
	
	self
		assert: (match at: dirFlag) value
		equals: (Path * 'foo' / 'bar' / 'baz').

]

{ #category : #running }
ClapMatchesTest >> testRootPositionalBeforeSubcommand [
	| match |
	match := clap match: #('clap' '--directory' 'foo/bar/baz' 'this').
	
	self deny: match isMismatch.
	self assert: (match includesMatchNamed: 'directory').
	self assert: (match includesMatchNamed: 'this').
	self deny: (match includesMatchNamed: 'that').
	self
		assert: ((match atName: 'directory') atName: 'DIR') value
		equals: 'foo/bar/baz'.
	self
		assert: (match at: dirFlag) positionalValues asArray
		equals: #( 'foo/bar/baz' )

]

{ #category : #running }
ClapMatchesTest >> testSubcommandAccessing [
	| match |
	match := clap match: #('clap' 'this').
	
	self deny: match isMismatch.
	self deny: (match includesMatchNamed: 'directory').
	self assert: (match includesMatchNamed: 'this').
	self deny: (match includesMatchNamed: 'that').
	self deny: (match atName: 'this') isMismatch.
	self deny: ((match atName: 'this') includesMatchNamed: 'slow').
	
	self
		should: [ match atName: 'this' ifFound: [ Notification signal ] ]
		raise: Notification.
	self
		should: [ match atName: 'this' ifFound: [ Notification signal ] ifAbsent: [ self fail ] ]
		raise: Notification.
]

{ #category : #running }
ClapMatchesTest >> testThis [
	| match |
	match := clap match: #('clap' 'this').
	
	self deny: match isMismatch.
	self deny: (match includesMatchNamed: 'directory').
	self assert: (match includesMatchNamed: 'this').
	self deny: (match includesMatchNamed: 'that').
	self deny: (match atName: 'this') isMismatch.
	self deny: ((match atName: 'this') includesMatchNamed: 'slow')
]

{ #category : #running }
ClapMatchesTest >> testWrongCommandMismatches [
	| match |
	match := clap match: #('foo' '-d' 'bar/baz/qux' 'this').
	
	self assert: match isMismatch.
	self
		assert: match word
		equals: 'foo'
]
