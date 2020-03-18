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
	hello := (ClapCommand id: #hello)
		add: (ClapFlag id: #shout);
		add: (ClapPositional id: #who)

]

{ #category : #'tests - matching' }
ClapHelloWorldTest >> testHello [
	| match |
	match := hello match: #('hello').

	self deny: match isMismatch.
	self deny: (match includesMatchOf: #shout).
	self deny: (match includesMatchOf: #who)
]

{ #category : #'tests - matching' }
ClapHelloWorldTest >> testHelloWorld [
	| match |
	match := hello match: #('hello' 'world').

	self deny: match isMismatch.
	self deny: (match includesMatchOf: #shout).
	self assert: (match includesMatchOf: #who).
	self
		assert: (match at: #who) word
		equals: 'world'.
	self
		assert: (match at: #who) parent
		identicalTo: match
]

{ #category : #'tests - matching' }
ClapHelloWorldTest >> testLanguageFlag [
	| match lang |
	lang := ClapPositional id: #language.
	hello add: ((ClapFlag id: #lang)
		add: lang;
		meaning: [ :flag | (flag at: lang) value]).

	match := hello match: #('hello' 'monde' '--lang' 'fr' '--shout').

	self deny: match isMismatch.
	self assert: (match includesMatchOf: #shout).
	self assert: (match includesMatchOf: #who).
	self
		assert: (match at: #who) word
		equals: 'monde'.
	self assert: (match includesMatchOf: #lang).
	self assert: ((match at: #lang) includesMatchOf: #language).
	self
		assert: (match at: #lang) value
		equals: 'fr'.
	self flag: 'needs a context'. "((match atName: 'lang') at: lang) context should be: match."
]

{ #category : #'tests - matching' }
ClapHelloWorldTest >> testShouting [
	| match |
	match := hello match: #('hello' '--shout').

	self deny: match isMismatch.
	self assert: (match includesMatchOf: #shout).
	self deny: (match includesMatchOf: #who)
]

{ #category : #'tests - matching' }
ClapHelloWorldTest >> testShoutingAfterthought [
	| match |
	match := hello match: #('hello' 'world' '-s').

	self deny: match isMismatch.
	self assert: (match includesMatchOf: #shout).
	self assert: (match includesMatchOf: #who)
]

{ #category : #'tests - matching' }
ClapHelloWorldTest >> testShoutingWorld [
	| match |
	match := hello match: #('hello' '--shout' 'world').

	self deny: match isMismatch.
	self assert: (match includesMatchOf: #shout).
	self assert: (match includesMatchOf: #who)
]
