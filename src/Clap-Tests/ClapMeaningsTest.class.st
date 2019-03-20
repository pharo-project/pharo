Class {
	#name : #ClapMeaningsTest,
	#superclass : #TestCase,
	#category : #'Clap-Tests-Integration'
}

{ #category : #tests }
ClapMeaningsTest >> testArgFlagBasicMeaning [
	| flag match |
	flag := (ClapFlag withName: 'foo')
		add: (ClapPositional withName: 'bar').

	match := flag match: #('--foo' 'whatever').
	
	self assert: match value.
	self deny: (flag match: #()) value
]

{ #category : #tests }
ClapMeaningsTest >> testArgFlagCollectionMeaning [
	| flag match |
	flag := (ClapFlag withName: 'foo')
		add: ((ClapPositional withName: 'bar') implicitMeaning: [ #nobar ]);
		add: ((ClapPositional withName: 'baz') implicitMeaning: [ #nobaz ]);
		meaningCollection.
	match := flag match: #('--foo' 'whatever 1' 'whatever 2').
	self assert: match value asArray equals: #('whatever 1' 'whatever 2').
	match := flag match: #('--foo' 'whatever').
	self assert: match value asArray equals: #('whatever' nobaz).
	match := flag match: #('--foo').
	self assert: match value asArray equals: #(nobar nobaz)
]

{ #category : #tests }
ClapMeaningsTest >> testArgFlagScalarMeaning [
	| flag match |
	flag := (ClapFlag withName: 'foo')
		add: ((ClapPositional withName: 'bar') implicitMeaning: [ 'oops' ]);
		meaningScalar.
	match := flag match: #('--foo' 'whatever').
	self assert: match value equals: 'whatever'.
	match := flag match: #('--foo').
	self assert: match value equals: 'oops'.
	match := flag match: #().	"might be wrong, since this is really a mismatch"
	self assert: match value equals: 'oops'
]

{ #category : #tests }
ClapMeaningsTest >> testSimpleFlagBasicMeaning [
	| flag match |
	flag := ClapFlag withName: 'foo'.

	match := flag match: #('--foo').
	
	self assert: match value.
	self deny: (flag match: #()) value
]
