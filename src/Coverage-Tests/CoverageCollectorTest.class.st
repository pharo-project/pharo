Class {
	#name : 'CoverageCollectorTest',
	#superclass : 'TestCase',
	#category : 'Coverage-Tests',
	#package : 'Coverage-Tests'
}

{ #category : 'example' }
CoverageCollectorTest >> methodCallMethods [

	self methodEmpty.
	self methodOnlyReturn.
	self methodTwoPaths: true.
	self methodManyBlocks
]

{ #category : 'example' }
CoverageCollectorTest >> methodCallee [
]

{ #category : 'example' }
CoverageCollectorTest >> methodEmpty [
]

{ #category : 'example' }
CoverageCollectorTest >> methodManyBlocks [

	| i |
	i := 5.
	[ 1. i > 0 ] whileTrue: [ 2. i := i - 1 ].
	[ 3. i > 0 ] whileTrue: [ "not called" 4. i := i - 1 ].
	{ 0. 1 } collect: [ :j |
		| block |
		{ 0. 1 } do: [ :x | 6. x ].
		{ } do: [ :x | "not called" 7. x ].
		block := [ 8. self methodCallee ].
		block value.
		block := [ ].
		block value.
		block := [ "not called" 9. ].
		block := [ ] "not called".
		self methodCallee.
		j ]
]

{ #category : 'example' }
CoverageCollectorTest >> methodNotCalled [

	"Not expected to be called"

	self methodCallee.
]

{ #category : 'example' }
CoverageCollectorTest >> methodOnlyReturn [

	^1
]

{ #category : 'example' }
CoverageCollectorTest >> methodTwoPaths: aBoolean [

	aBoolean ifTrue: [ 1 ].
	aBoolean ifTrue: [ ^2 ] ifFalse: [ 3 ].
	^4.
]

{ #category : 'tests' }
CoverageCollectorTest >> testBasicCoverage [

	| cov res |
	cov := CoverageCollector new.
	cov methods: { (Rectangle >> #width) . (Rectangle >> #area) . (Rectangle >> #intersect:) }.
	res := cov runOn: [ (1 @ 1 corner: 2 @ 2) area ].
	self deny: cov metalink hasNodes. "metaliks where removed"
	self assertCollection: res methods hasSameElements: { (Rectangle >> #width) . (Rectangle >> #area) }.
	self assert: res uncoveredMethods asArray equals: { (Rectangle >> #intersect:) }
]

{ #category : 'tests' }
CoverageCollectorTest >> testCallgraph [

	| cov graph |
	cov := CoverageCollector new.
	cov methods: (self class methods select: [ :m | 'method*' match: m selector ]).
	graph := cov callgraph.
	self assert: graph size equals: 21
]

{ #category : 'tests' }
CoverageCollectorTest >> testExample [

	| collector coverage |
	collector := CoverageCollector new. "Instantiate"
	"We skip the method with special selectors because them might not be detected by the coverage collector in case they are optimized by the VM."
	collector methods: (Point methods , Rectangle methods reject: [ :method | BytecodeEncoder specialSelectors includes: method selector ]). "Configure with the methods to watch."
	coverage := collector runOn: [ (1 @ 1 corner: 2 @ 2) center ]. "Setup, execute and teardown."
	self
		assertCollection: (coverage methods collect: [ :method | method name ])
		hasSameElements:
			#( 'Point>>#corner:' 'Point>>#isPoint' 'Rectangle>>#setPoint:point:' 'Rectangle>>#topLeft'
			   'Rectangle>>#bottomRight' 'Rectangle>>#center' ). "Inspect the results"
	self assert: coverage nodes size equals: 6 "Covered paths are also available"
]

{ #category : 'tests' }
CoverageCollectorTest >> testNoCoverage [

	| cov res |
	cov := CoverageCollector new.
	cov methods add: Rectangle >> #closeTo:.
	res := cov runOn: [ (1 @ 1 corner: 2 @ 2) area ].
	self assertEmpty: res methods.
	self assert: res percentage equals: 0
]

{ #category : 'tests' }
CoverageCollectorTest >> testNodeCoverage [

	| cov res |
	cov := CoverageCollector new.
	cov methods add: self class>>#methodTwoPaths:.

	res := cov runOn: [ self methodTwoPaths: false. self methodTwoPaths: false. ].
	self deny: cov metalink hasNodes. "metaliks where removed"
	self assert: cov methods size equals: 1.
	self assert: cov nodes size equals: 4.

	self assert: res methods size equals: 1.
	self assert: res nodes size equals: 2.
	self assert: (cov nodes includesAll: res nodes).
	self assert: res percentage asInteger equals: 50.
	self assert: res uncoveredNodes size equals: 2.
	
	res := cov runOn: [ self methodTwoPaths: true. self methodTwoPaths: true. ].
	self assert: (cov nodes includesAll: res nodes).
	self assert: res methods size equals: 1.
	self assert: res nodes size equals: 3.
	self assert: res percentage asInteger equals: 75.
	self assert: res uncoveredNodes size equals: 1.

	res := cov runOn: [ self methodTwoPaths: true. self methodTwoPaths: false. ].
	self assert: (cov nodes includesAll: res nodes).
	self assert: res methods size equals: 1.
	self assert: res nodes size equals: 4.
	self assert: res percentage asInteger equals: 100.
	self assertEmpty: res uncoveredNodes

]

{ #category : 'tests' }
CoverageCollectorTest >> testNodeCoverage2 [

	| cov res |
	cov := CoverageCollector new.
	cov methods: (self class methods select: [ :m | 'method*' match: m selector ]).

	res := cov runOn: [ self methodCallMethods ].
	self deny: cov metalink hasNodes. "metaliks where removed"
	self assert: cov methods size equals: 7.
	self assert: cov nodes size equals: 21.

	self assert: res methods size equals: 6.
	self assert: res nodes size equals: 15.
	self assert: (cov nodes includesAll: res nodes).
	self assert: res uncoveredNodes size equals: 6.
]

{ #category : 'tests' }
CoverageCollectorTest >> testNodeDistancesFrom [

	| cov graph target distances |
	cov := CoverageCollector new.
	cov methods: (self class methods select: [ :m | 'method*' match: m selector ]).
	graph := cov callgraph.
		
	target := (self class >> #methodManyBlocks) ast body.
	distances := cov nodeDistancesFrom: target.
	self assert: (distances values sorted) equals: { 0. 1. 1. 1. 1. 1. 2. 2. 2. 2. 2. 2. 2}.
]

{ #category : 'tests' }
CoverageCollectorTest >> testNodeDistancesTo [

	| cov graph target distances |
	cov := CoverageCollector new.
	cov methods: (self class methods select: [ :m | 'method*' match: m selector ]).
	graph := cov callgraph.
		
	target := (self class >> #methodCallee) ast body.
	distances := cov nodeDistancesTo: target.
	self assert: (distances values sorted) equals: { 0. 1. 1. 1. 2. 3}.
]

{ #category : 'tests' }
CoverageCollectorTest >> testNothingToCover [

	| cov res |
	cov := CoverageCollector new.
	res := cov runOn: [ (1 @ 1 corner: 2 @ 2) area ].
	self assertEmpty: res methods.
	self assert: res percentage isNaN
]
