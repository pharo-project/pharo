Class {
	#name : #HiNodesAndLinksIteratorWithTwoLinkModelsTest,
	#superclass : #TestCase,
	#instVars : [
		'a',
		'b',
		'c',
		'd',
		'expectedArray',
		'reversedNodes',
		'parentsLinkBuilder',
		'originLinkBuilder'
	],
	#category : #HiedraTests
}

{ #category : #running }
HiNodesAndLinksIteratorWithTwoLinkModelsTest >> assertIteratedAsExpected [
	| builtArray |
	builtArray := self iteratedAsArray.
	
	self assert: builtArray equals: expectedArray
]

{ #category : #running }
HiNodesAndLinksIteratorWithTwoLinkModelsTest >> iteratedAsArray [
	| result |
	result := OrderedCollection new.	
		
	HiNodesAndLinksIterator new 
		linkBuilders: { parentsLinkBuilder. originLinkBuilder };
		values: reversedNodes reversed;
		doWithNodes: [ :item | result add: item ]
		doWithLinks: [ :link | result add: link ].
	
	^ result asArray
]

{ #category : #running }
HiNodesAndLinksIteratorWithTwoLinkModelsTest >> setUp [
	super setUp.
	
	parentsLinkBuilder := HiLinkBuilder new
		label: #parents;
		targetsBlock: [ :node | node parents ];
		yourself.
	originLinkBuilder := HiLinkBuilder new
		label: #origin;
		targetsBlock: [ :node | 
			node origin 
				ifNil: [ #() ]
				ifNotNil: [ { node origin } ] ];
		yourself.
]

{ #category : #tests }
HiNodesAndLinksIteratorWithTwoLinkModelsTest >> testBasicExpected [
	"Secondary (i.e. origin) links are iterated after all primary (i.e. parents) links are iterated."

	"
	c
	|\\ 
	| || 
	| b 
	| | 
	|/ 
	a 	
	"
		
	reversedNodes := {
		a := HiTestNode content: $a parents: {}.
		b := HiTestNode content: $b parents: { a }.	
		c := (HiTestNode content: $c parents: { a }) origin: b; yourself
		}.
	
	expectedArray := {
		HiNode contents: c. 
		HiNode contents: a. 
		HiLink linkBuilder: parentsLinkBuilder origin: (HiNode contents: c) target: (HiNode contents: a). 
		HiNode contents: b. 
		HiLink linkBuilder: parentsLinkBuilder origin: (HiNode contents: b) target: (HiNode contents: a). 
		HiLink linkBuilder: originLinkBuilder  origin: (HiNode contents: c) target: (HiNode contents: b) }.
	
	self assertIteratedAsExpected.
]
