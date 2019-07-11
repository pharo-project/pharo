Class {
	#name : #HiRulerBuilderTest,
	#superclass : #TestCase,
	#instVars : [
		'ruler'
	],
	#classVars : [
		'DebugShowingRulerInWindow'
	],
	#category : #'Hiedra-Tests-Model'
}

{ #category : #'initialize - event' }
HiRulerBuilderTest class >> initialize [
	DebugShowingRulerInWindow := false
]

{ #category : #running }
HiRulerBuilderTest >> assertRulerHasLinks: expectedLinksAndRulerPointsAsAssociations [

	| actualRulerPoints |
	self assert: ruler links size equals: expectedLinksAndRulerPointsAsAssociations size.

	expectedLinksAndRulerPointsAsAssociations withIndexDo: [ :each :index |
		| actualLink expectedLinkAsAssociation rulerPoints |
		actualLink := ruler links at: index.
		expectedLinkAsAssociation := each key.
		rulerPoints := each value.
		actualRulerPoints := {actualLink origin rulerPoint}, actualLink intermediatePoints asArray, {actualLink target rulerPoint}.
		self
			assert: actualLink origin theValue -> actualLink target theValue
			equals: expectedLinkAsAssociation.
		self
			assert: actualRulerPoints = rulerPoints asArray
			description: [ '{1}: Found {2} instead of {3}.' format: { actualLink. actualRulerPoints. rulerPoints } ].
	]

]

{ #category : #running }
HiRulerBuilderTest >> assertRulerHasNodes: expectedNodesAndRulerPointsAsAssociations [

	| actualNodes failures |
	actualNodes := ruler nodes.
	self assert: actualNodes size equals: expectedNodesAndRulerPointsAsAssociations size.
	self
		assert: (actualNodes collect: #theValue as: Bag)
		equals: (expectedNodesAndRulerPointsAsAssociations collect: #key as: Bag).

	failures := expectedNodesAndRulerPointsAsAssociations select: [ :each |
		| actualNode expectedValue expectedRulerPoint|
		expectedValue := each key.
		expectedRulerPoint := each value.
		actualNode := actualNodes detect: [:aNode | aNode theValue = expectedValue ].
		actualNode rulerPoint ~= expectedRulerPoint.
		].

	self
		assert: failures isEmpty
		description: [ 'Nodes found at unexpected points: {1}'
			format: { failures printString } ].

]

{ #category : #running }
HiRulerBuilderTest >> buildRulerWith: valuesAndParentsAsAssociations [

	| valuesAndParentsAsDictionary values |

	"This dictionary keeps order, we need it."
	valuesAndParentsAsDictionary := valuesAndParentsAsAssociations as: SmallDictionary.
	values := valuesAndParentsAsDictionary keys.
	
	ruler := HiRulerBuilder new 
		values: values;
		linksBlock: [ :key | valuesAndParentsAsDictionary at: key ];
		build;
		ruler.

	self debugShowingRulerInPresenter: valuesAndParentsAsDictionary.
]

{ #category : #running }
HiRulerBuilderTest >> debugShowingRulerInPresenter: valuesAndParentsAsDictionary [
	"Open a Spec window when a class variable enables it.

	Enable with:
		DebugShowingRulerInWindow := true.
	Disable with:
		DebugShowingRulerInWindow := false.
	"

	| hiedraColumnController values |
	DebugShowingRulerInWindow ifFalse: [ ^ self ].

	values := valuesAndParentsAsDictionary keys.

	"Set up the ruler controller."
	hiedraColumnController := HiColumnController new.
	hiedraColumnController ruler: (HiRulerBuilder newRulerValues: values linksBlock: [ :key | valuesAndParentsAsDictionary at: key ]).
	hiedraColumnController renderer
		rowHeight: FTTableMorph defaultRowHeight floor;
		cellWidth: 10.
	hiedraColumnController reset.

	"Show the ruler in a Spec tree."
	SpTablePresenter new
		addColumn:
			((SpStringTableColumn evaluated: #value)
				width: 15;
				yourself);
		addColumn: (SpImageTableColumn evaluated: [ :item | hiedraColumnController cellMorphAtValue: item ]);
		items: values;
		title: testSelector asString;
		openWithSpec
]

{ #category : #tests }
HiRulerBuilderTest >> testCycle [
	"
	b
	| \
	| /
	a
	"

	self buildRulerWith: {
		#b -> #(a).
		#a -> #(b).
	}.

	self assertRulerHasNodes: {
		#b -> (1@1).
		#a -> (1@2).
	}.
	
	self assertRulerHasLinks: {
		(#b->#a) -> {1@1. 1@2}.
		(#a->#b) -> {1@2. 1@1}.
	}.
]

{ #category : #tests }
HiRulerBuilderTest >> testEmpty [
	self buildRulerWith: #().

	self assertEmpty: ruler nodes.
	self assertEmpty: ruler links
]

{ #category : #tests }
HiRulerBuilderTest >> testFourMergedForks [
	"
	d
	|\ \
	| | | 
	| c | 
	| |\| 
	| |/\
	| b |
	| | | 
	|/ /
	a
	"
	
	self buildRulerWith: {
		#d -> #(a c b).
		#c -> #(b a).
		#b -> #(a).
		#a -> #().
	}.

	self assertRulerHasNodes: {
		#d -> (1@1).
		#c -> (2@2).
		#b -> (2@3).
		#a -> (1@4).
	}.
	
	self assertRulerHasLinks: {
		(#d->#a) -> {1@1. 1@2. 1@3. 1@4}.

		(#d->#c) -> {1@1. 2@2}.
		(#c->#b) -> {2@2. 2@3}.
		(#b->#a) -> {2@3. 1@4}.

		(#c->#a) -> {2@2. 3@3. 1@4}.

		(#d->#b) -> {1@1. 3@2. 2@3}.
	}.

]

{ #category : #tests }
HiRulerBuilderTest >> testOneNode [
	"
	a
	"

	self buildRulerWith: {
		#a -> #().
	}.

	self assertRulerHasNodes: {
		#a -> (1@1).
	}.
	
	self assertEmpty: ruler links.
]

{ #category : #tests }
HiRulerBuilderTest >> testThreeMergedForks [
	"
	d
	|\ 
	| | 
	c | 
	|\| 
	|/\
	b |
	| | 
	|/ 
	a
	"
	
	self buildRulerWith: {
		#d -> #(c b).
		#c -> #(b a).
		#b -> #(a).
		#a -> #().
	}.

	self assertRulerHasNodes: {
		#d -> (1@1).
		#c -> (1@2).
		#b -> (1@3).
		#a -> (1@4).
	}.
	
	self assertRulerHasLinks: {
		(#d->#c) -> {1@1. 1@2}.
		(#c->#b) -> {1@2. 1@3}.
		(#b->#a) -> {1@3. 1@4}.

		(#c->#a) -> {1@2. 2@3. 1@4}.

		(#d->#b) -> {1@1. 2@2. 1@3}.
	}.

]

{ #category : #tests }
HiRulerBuilderTest >> testThreeMergedForks2 [
	"
	d
	|\ 
	| | 
	| c 
	| | 
	|/| 
	b |
	| | 
	|/ 
	a
	"
	
	self buildRulerWith: {
		#d -> #(b c).
		#c -> #(b a).
		#b -> #(a).
		#a -> #().
	}.

	self assertRulerHasNodes: {
		#d -> (1@1).
		#c -> (2@2).
		#b -> (1@3).
		#a -> (1@4).
	}.
	
	self assertRulerHasLinks: {
		(#d->#b) -> {1@1. 1@2. 1@3}.
		(#b->#a) -> {1@3. 1@4}.

		(#d->#c) -> {1@1. 2@2}.
		(#c->#b) -> {2@2. 1@3}.

		(#c->#a) -> {2@2. 2@3. 1@4}.
	}.

]

{ #category : #tests }
HiRulerBuilderTest >> testThreeMergedForks3 [
	"
	d
	|\ 
	| | 
	| c 
	| | 
	|/| 
	b |
	| | 
	|/ 
	a
	"
	
	self buildRulerWith: {
		#d -> #(b c).
		#c -> #(a b).
		#b -> #(a).
		#a -> #().
	}.

	self assertRulerHasNodes: {
		#d -> (1@1).
		#c -> (2@2).
		#b -> (1@3).
		#a -> (1@4).
	}.
	
	self assertRulerHasLinks: {
		(#d->#b) -> {1@1. 1@2. 1@3}.
		(#b->#a) -> {1@3. 1@4}.

		(#d->#c) -> {1@1. 2@2}.
		(#c->#a) -> {2@2. 2@3. 1@4}.

		(#c->#b) -> {2@2. 1@3}.
	}.

]

{ #category : #tests }
HiRulerBuilderTest >> testTwoConnectedNodes [
	"
	b
	|
	a
	"

	self buildRulerWith: {
		#b -> #(a).
		#a -> #().
	}.

	self assertRulerHasNodes: {
		#b -> (1@1).
		#a -> (1@2).
	}.
	
	self assertRulerHasLinks: {
		(#b->#a) -> {1@1. 1@2}.
	}.
]

{ #category : #tests }
HiRulerBuilderTest >> testTwoMergedForks [
	"
	c
	|\ 
	| | 
	| b 
	| | 
	|/ 
	a 
	"
	
	self buildRulerWith: {
		#c -> #(a b).
		#b -> #(a).
		#a -> #().
	}.

	self assertRulerHasNodes: {
		#c -> (1@1).
		#b -> (2@2).
		#a -> (1@3).
	}.
	
	self assertRulerHasLinks: {
		(#c->#a) -> {1@1. 1@2. 1@3}.

		(#c->#b) -> {1@1. 2@2}.
		(#b->#a) -> {2@2. 1@3}.
	}.

]

{ #category : #tests }
HiRulerBuilderTest >> testTwoMergedForks2 [
	"
	c
	|\ 
	| | 
	b | 
	| | 
	|/ 
	a 
	"
	
	self buildRulerWith: {
		#c -> #(b a).
		#b -> #(a).
		#a -> #().
	}.

	self assertRulerHasNodes: {
		#c -> (1@1).
		#b -> (1@2).
		#a -> (1@3).
	}.
	
	self assertRulerHasLinks: {
		(#c->#b) -> {1@1. 1@2}.
		(#b->#a) -> {1@2. 1@3}.

		(#c->#a) -> {1@1. 2@2. 1@3}.
	}.

]

{ #category : #tests }
HiRulerBuilderTest >> testTwoUnconnectedNodes [
	"
	b

	a	 	
	"

	self buildRulerWith: {
		#b -> #().
		#a -> #().
	}.

	self assertRulerHasNodes: {
		#b -> (1@1).
		#a -> (1@2).
	}.
	
	self assertRulerHasLinks: {
	}.

]

{ #category : #tests }
HiRulerBuilderTest >> testTwoUnmergedForks [
	"
	d
	|
	| c
	| |
	b |
	|/
	a
	"

	self buildRulerWith: {
		#d -> #(b).
		#c -> #(a).
		#b -> #(a).
		#a -> #().
	}.

	self assertRulerHasNodes: {
		#d -> (1@1).
		#b -> (1@3).
		#a -> (1@4).
		#c -> (2@2).
	}.
	
	self assertRulerHasLinks: {
		(#d->#b) -> {1@1. 1@2. 1@3}.
		(#b->#a) -> {1@3. 1@4}.

		(#c->#a) -> {2@2. 2@3. 1@4}.
	}.

]
