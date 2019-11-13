Class {
	#name : #MalHitsNodeTest,
	#superclass : #TestCase,
	#category : #'Moose-Algos-Graph-Tests'
}

{ #category : #tests }
MalHitsNodeTest >> testPrintString [
	|node|
	node := MalHitsNode new
		model: 'model';
		auth: 7;
		hub: 11;
		yourself.
	self assert: node printString equals: '(''model'' 7.00 11.00)'.
]
