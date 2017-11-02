Class {
	#name : #HiNodesAndLinksIteratorWithOneLinkModelTest,
	#superclass : #TestCase,
	#instVars : [
		'a',
		'b',
		'c',
		'd',
		'expectedArray',
		'reversedNodes',
		'linkBuilder'
	],
	#category : 'HiedraTests'
}

{ #category : #running }
HiNodesAndLinksIteratorWithOneLinkModelTest >> assertIteratedAsExpected [
	| builtArray |
	builtArray := self iteratedAsArray.
	
	self assert: builtArray equals: expectedArray
]

{ #category : #running }
HiNodesAndLinksIteratorWithOneLinkModelTest >> iteratedAsArray [
	| result |
	result := OrderedCollection new.	
		
	HiNodesAndLinksIterator new 
		linkBuilders: { linkBuilder };
		values: reversedNodes reversed;
		doWithNodes: [ :item | result add: item ]
		doWithLinks: [ :link | result add: link ].
	
	^ result asArray
]

{ #category : #running }
HiNodesAndLinksIteratorWithOneLinkModelTest >> setUp [
	linkBuilder := HiLinkBuilder new
		targetsBlock: [ :node | node parents ];
		yourself.
]

{ #category : #tests }
HiNodesAndLinksIteratorWithOneLinkModelTest >> testEmpty [

	reversedNodes := {}.
	expectedArray := {}.
	
	self assertIteratedAsExpected.
]

{ #category : #tests }
HiNodesAndLinksIteratorWithOneLinkModelTest >> testOneNode [
	"
	a
	"

	reversedNodes := {
		a := HiTestNode content: $a parents: {}.
		}.
	
	expectedArray := { HiNode contents: a }. 
		
	self assertIteratedAsExpected.
]

{ #category : #tests }
HiNodesAndLinksIteratorWithOneLinkModelTest >> testTwoConnectedNodes [
	"
	b
	|
	a
	"

	reversedNodes := {
		a := HiTestNode content: $a parents: {}.
		b := HiTestNode content: $b parents: { a }.	
		}.
	
	expectedArray := { 
		HiNode contents: b. 
		HiNode contents: a. 
		HiLink linkBuilder: linkBuilder origin: (HiNode contents: b) target: (HiNode contents: a) }.
		
	self assertIteratedAsExpected.
]

{ #category : #tests }
HiNodesAndLinksIteratorWithOneLinkModelTest >> testTwoMergedFork [
	"
	c
	|\ 
	| | 
	b | 
	| | 
	|/ 
	a 	
	"
		
	reversedNodes := {
		a := HiTestNode content: $a parents: {}.
		b := HiTestNode content: $b parents: { a }.	
		c := HiTestNode content: $c parents: { a. b } 
		}.
	
	expectedArray := { 
		HiNode contents: c. 
		HiNode contents: b. 
		HiNode contents: a. 
		HiLink linkBuilder: linkBuilder origin: (HiNode contents: b) target: (HiNode contents: a). 
		HiLink linkBuilder: linkBuilder origin: (HiNode contents: c) target: (HiNode contents: b). 
		HiLink linkBuilder: linkBuilder origin: (HiNode contents: c) target: (HiNode contents: a) }.
	
	self assertIteratedAsExpected.
]

{ #category : #tests }
HiNodesAndLinksIteratorWithOneLinkModelTest >> testTwoUnconnectedNodes [
	"
	b

	a	 	
	"

	reversedNodes := {
		a := HiTestNode content: $a parents: {}.
		b := HiTestNode content: $b parents: {}.	
		}.
	
	expectedArray := { 
		HiNode contents: b. 
		HiNode contents: a. }.
		
	self assertIteratedAsExpected.
]

{ #category : #tests }
HiNodesAndLinksIteratorWithOneLinkModelTest >> testTwoUnmergedForks [
	"
	c
	|
	|
	| b
	| |
	|/
	a
	"

	reversedNodes := {
		a := HiTestNode content: $a parents: {}.
		b := HiTestNode content: $b parents: { a }.	
		c := HiTestNode content: $c parents: { a }.	
		}.
	
	expectedArray := { 
		HiNode contents: c. 
		HiNode contents: a. 
		HiLink linkBuilder: linkBuilder origin: (HiNode contents: c) target: (HiNode contents: a). 
		HiNode contents: b. 
		HiLink linkBuilder: linkBuilder origin: (HiNode contents: b) target: (HiNode contents: a). }.
		
	self assertIteratedAsExpected.
]

{ #category : #tests }
HiNodesAndLinksIteratorWithOneLinkModelTest >> testTwoUnmergedForks2 [
	"
	d
	|
	| c
	| |
	b |
	|/
	a
	"

	reversedNodes := {
		a := HiTestNode content: $a parents: {}.
		b := HiTestNode content: $b parents: { a }.	
		c := HiTestNode content: $c parents: { a }.	
		d := HiTestNode content: $d parents: { b }.	
		}.
	
	expectedArray := { 
		HiNode contents: d. 
		HiNode contents: b. 
		HiNode contents: a. 
		HiLink linkBuilder: linkBuilder origin: (HiNode contents: b) target: (HiNode contents: a). 
		HiLink linkBuilder: linkBuilder origin: (HiNode contents: d) target: (HiNode contents: b). 
		HiNode contents: c. 
		HiLink linkBuilder: linkBuilder origin: (HiNode contents: c) target: (HiNode contents: a) }.
		
	self assertIteratedAsExpected.
]
