Class {
	#name : #ClapHelloWorldTest,
	#superclass : #TestCase,
	#instVars : [
		'hello'
	],
	#category : #'Clap-Tests-Integration'
}

{ #category : #running }
ClapHelloWorldTest >> setUp [
	super setUp.
	hello := (ClapCommand withName: 'hello')
		add: (ClapFlag withName: 'shout');
		add: (ClapPositional withName: 'who')

]

{ #category : #'tests - matching' }
ClapHelloWorldTest >> testHello [
	| match |
	match := hello match: #('hello').

	self deny: match isMismatch.
	self deny: (match includesMatchNamed: 'shout').
	self deny: (match includesMatchNamed: 'who')
]

{ #category : #'tests - matching' }
ClapHelloWorldTest >> testHelloWorld [
	| match |
	match := hello match: #('hello' 'world').

	self deny: match isMismatch.
	self deny: (match includesMatchNamed: 'shout').
	self assert: (match includesMatchNamed: 'who').
	self
		assert: (match atName: 'who') word
		equals: 'world'.
	self
		assert: (match atName: 'who') parent
		identicalTo: match
]

{ #category : #'tests - matching' }
ClapHelloWorldTest >> testLanguageFlag [
	| match lang |
	lang := ClapPositional withName: 'language'.
	hello add: ((ClapFlag withName: 'lang')
		add: lang;
		meaning: [ :flag | (flag at: lang) value]).

	match := hello match: #('hello' 'monde' '--lang' 'fr' '--shout').

	self deny: match isMismatch.
	self assert: (match includesMatchNamed: 'shout').
	self assert: (match includesMatchNamed: 'who').
	self
		assert: (match atName: 'who') word
		equals: 'monde'.
	self assert: (match includesMatchNamed: 'lang').
	self assert: ((match atName: 'lang') includesMatchNamed: 'language').
	self
		assert: (match atName: 'lang') value
		equals: 'fr'.
	self flag: 'needs a context'. "((match atName: 'lang') at: lang) context should be: match."
]

{ #category : #'tests - matching' }
ClapHelloWorldTest >> testShouting [
	| match |
	match := hello match: #('hello' '--shout').

	self deny: match isMismatch.
	self assert: (match includesMatchNamed: 'shout').
	self deny: (match includesMatchNamed: 'who')
]

{ #category : #'tests - matching' }
ClapHelloWorldTest >> testShoutingAfterthought [
	| match |
	match := hello match: #('hello' 'world' '-s').

	self deny: match isMismatch.
	self assert: (match includesMatchNamed: 'shout').
	self assert: (match includesMatchNamed: 'who')
]

{ #category : #'tests - matching' }
ClapHelloWorldTest >> testShoutingWorld [
	| match |
	match := hello match: #('hello' '--shout' 'world').

	self deny: match isMismatch.
	self assert: (match includesMatchNamed: 'shout').
	self assert: (match includesMatchNamed: 'who')
]
