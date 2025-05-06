Class {
	#name : 'ClapMeaningsTest',
	#superclass : 'TestCase',
	#category : 'Clap-Tests-Integration',
	#package : 'Clap-Tests',
	#tag : 'Integration'
}

{ #category : 'tests' }
ClapMeaningsTest >> testArgFlagBasicMeaning [
	| flag match |
	flag := (ClapFlag id: #foo)
		add: (ClapPositional id: #bar);
		yourself.

	match := flag match: #('--foo' 'whatever').
	
	self assert: match isMatch.
	self assert: (flag match: #()) isMismatch
]

{ #category : 'tests' }
ClapMeaningsTest >> testArgFlagCollectionMeaning [
	| flag match |
	flag := (ClapFlag id: #foo)
		add: ((ClapPositional id: #bar) implicitMeaning: [ #nobar ]);
		add: ((ClapPositional id: #baz) implicitMeaning: [ #nobaz ]);
		meaningCollection.
	match := flag match: #('--foo' 'whatever 1' 'whatever 2').
	self assert: match value asArray equals: #('whatever 1' 'whatever 2').
	match := flag match: #('--foo' 'whatever').
	self assert: match value asArray equals: #('whatever' nobaz).
	match := flag match: #('--foo').
	self assert: match value asArray equals: #(nobar nobaz)
]

{ #category : 'tests' }
ClapMeaningsTest >> testArgFlagScalarMeaning [
	| flag match |
	flag := (ClapFlag id: #foo)
		add: ((ClapPositional id: #bar) implicitMeaning: [ 'oops' ]);
		meaningScalar.
	match := flag match: #('--foo' 'whatever').
	self assert: match value equals: 'whatever'.
	match := flag match: #('--foo').
	self assert: match value equals: 'oops'.
	match := flag match: #().	"might be wrong, since this is really a mismatch"
	self assert: match isMismatch.
]

{ #category : 'tests' }
ClapMeaningsTest >> testSimpleFlagBasicMeaning [
	| flag match |
	flag := ClapFlag id: #foo.

	match := flag match: #('--foo').
	
	self assert: match isMatch.
	self deny: (flag match: #()) isMatch
]
