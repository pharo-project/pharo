Class {
	#name : #MalDominanceTest,
	#superclass : #TestCase,
	#instVars : [
		'dom'
	],
	#category : #'Moose-Algos-Graph-Tests'
}

{ #category : #running }
MalDominanceTest >> setUp [
	super setUp.
	dom := MalDominance new
]

{ #category : #tests }
MalDominanceTest >> testCycle [
	| a b c |
	MalGraphFixture cycleGraph: dom.
	dom run.
	dom nodes
		do: [ :n | 
			n model = $c
				ifTrue: [
					self assert: n isDominant.
					self deny: n isDominated ]
				ifFalse: [ self deny: n isDominant.
					n model = $d
						ifTrue: [ self deny: n isDominated ]
						ifFalse: [ self assert: n isDominated ] ] ].
	a := dom findNode: $a.
	b := dom findNode: $b.
	c := dom findNode: $c.
	self assert: a find equals: c.
	self assert: b find equals: c
]

{ #category : #tests }
MalDominanceTest >> testSimple [
	| a b c d |
	MalGraphFixture simpleGraph: dom.
	dom run.
	dom nodes
		do: [ :n | 
			n model = $a
				ifTrue: [ 
					self assert: n isDominant.
					self deny: n isDominated ]
				ifFalse: [ self deny: n isDominant ] ].
	a := dom findNode: $a.
	b := dom findNode: $b.
	c := dom findNode: $c.
	d := dom findNode: $d.
	self assert: b find equals: a.
	self deny: c isDominated.
	self deny: d isDominated
]
