"
A HiRulerTest is a test class for testing the behavior of HiRuler
"
Class {
	#name : #HiRulerTest,
	#superclass : #TestCase,
	#category : #'Hiedra-Tests-Model'
}

{ #category : #tests }
HiRulerTest >> testEmpty [
	| ruler |
	ruler := HiRuler withValues: #().

	self assertEmpty: ruler nodes.
	self assertEmpty: ruler links.
	self assertEmpty: ruler elementsByRow.
	self assert: ruler numberOfColumns equals: 0.
	self assert: ruler numberOfRows equals: 0
]

{ #category : #tests }
HiRulerTest >> testTwoMergedForks [
	"
	c
	|\ 
	| | 
	| b 
	| | 
	|/ 
	a 
	"

	| ruler c b a |
	ruler := HiRuler withValues: #(c b a).

	c := ruler addNodeFor: #c.
	a := ruler addNodeFor: #a.
	ruler addLinkFrom: c to: a.

	b := ruler addNodeFor: #b.
	ruler
		addLinkFrom: c to: b;
		addLinkFrom: b to: a.

	self assertCollection: ruler nodes hasSameElements: {c . b . a}.
	self assert: ruler links size equals: 3.

	self assert: c rulerPoint equals: 1 @ 1.
	self assert: b rulerPoint equals: 2 @ 2.
	self assert: a rulerPoint equals: 1 @ 3.

	self assert: c theValue equals: #c.
	self assert: b theValue equals: #b.
	self assert: a theValue equals: #a
]
