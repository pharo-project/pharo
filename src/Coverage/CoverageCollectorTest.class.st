Class {
	#name : #CoverageCollectorTest,
	#superclass : #TestCase,
	#category : #'Coverage-Tests'
}

{ #category : #example }
CoverageCollectorTest >> methodTwoPaths: aBoolean [

	aBoolean ifTrue: [ 1 ].
	aBoolean ifTrue: [ ^2 ] ifFalse: [ 3 ].
	^4.
]

{ #category : #tests }
CoverageCollectorTest >> testBasicCoverage [

	| cov res |
	cov := CoverageCollector new.
	cov methods add: Rectangle>>#width.
	cov methods add: Rectangle>>#area.
	cov methods add: Rectangle>>#intersect:.
	res := cov runOn: [ (1@1 corner: 2@2) area ].
	self assert: cov metalink hasNodes not. "metaliks where removed"
	self assert: res methods size equals: 2.
	self assert: res uncoveredMethods size equals: 1.
]

{ #category : #tests }
CoverageCollectorTest >> testNoCoverage [

	| cov res |
	cov := CoverageCollector new.
	cov methods add: Rectangle>>#closeTo:.
	res := cov runOn: [ (1@1 corner: 2@2) area ].
	self assert: res methods size equals: 0.
	self assert: res percentage equals: 0.
]

{ #category : #tests }
CoverageCollectorTest >> testNodeCoverage [

	| cov res |
	cov := CoverageCollector new.
	cov methods add: self class>>#methodTwoPaths:.

	res := cov runOn: [ self methodTwoPaths: false. self methodTwoPaths: false. ].
	self assert: cov metalink hasNodes not. "metaliks where removed"
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
	self assert: res uncoveredNodes size equals: 0.

]

{ #category : #tests }
CoverageCollectorTest >> testNothingToCover [

	| cov res |
	cov := CoverageCollector new.
	res := cov runOn: [ (1@1 corner: 2@2) area ].
	self assert: res methods size equals: 0.
	self assert: res percentage isNaN.
]
