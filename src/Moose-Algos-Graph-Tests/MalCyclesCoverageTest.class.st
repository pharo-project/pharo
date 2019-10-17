Class {
	#name : #MalCyclesCoverageTest,
	#superclass : #TestCase,
	#instVars : [
		'cc'
	],
	#category : #'Moose-Algos-Graph-Tests'
}

{ #category : #running }
MalCyclesCoverageTest >> setUp [
	super setUp.
	cc := MalCyclesCoverage new
]

{ #category : #'tests-cse' }
MalCyclesCoverageTest >> testAseCircuitWithCse [
	cc useCircuitSpan.
	MalGraphFixture aseCircuitGraph: cc.
	cc run.
	self assert: 5 equals: cc circuits size.
	#(#($g $h) #($b $d) #($a $b) #($b $c $d) #($a $b $d $e))
		do: [ :c | self assert: (cc circuits anySatisfy: [ :ci | c hasEqualElements: (cc normalize: ci) ]) ]
]

{ #category : #'tests-sed' }
MalCyclesCoverageTest >> testAseCircuitWithSed [
	cc useEdgeDecomposition.
	MalGraphFixture aseCircuitGraph: cc.
	cc run.
	self assert: 5 equals: cc circuits size.
	#(#($g $h) #($b $d) #($a $b) #($b $c $d) #($a $b $d $e))
		do: [ :c | self assert: (cc circuits anySatisfy: [ :ci | c hasEqualElements: ci ]) ]
]

{ #category : #'tests-snd' }
MalCyclesCoverageTest >> testAseCircuitWithSnd [
	MalGraphFixture aseCircuitGraph: cc.
	cc run.
	self assert: 5 equals: cc circuits size.
	#(($g $h) ($b $d) ($a $b) ($b $c $d) ($a $b $d $e)) do: [:c|
			self assert: (cc circuits anySatisfy: [:ci| c hasEqualElements: ci ])].

]

{ #category : #'tests-cse' }
MalCyclesCoverageTest >> testAseSccWithCse [
	| cse |
	cse := MalCircuitSpanningEdges new.
	MalGraphFixture aseSccGraph: cse.
	cse run.
	self assert: 4 equals: cse circuits size.
	self assert: (#($a $b) hasEqualElements: cse circuits first).
	self assert: (#($b $d) hasEqualElements: cse circuits second).
	self assert: (#($c $d $b) hasEqualElements: cse circuits third).
	self assert: (#($e $a $b $d) hasEqualElements: cse circuits fourth)
]

{ #category : #'tests-sed' }
MalCyclesCoverageTest >> testAseSccWithSed [
	| sed circuits |
	sed := MalSccEdgeDecomposition new.
	MalGraphFixture aseSccGraph: sed.
	sed run.
	self assert: 4 equals: sed circuits size.
	circuits := sed circuits asOrderedCollection.
	#(#($b $d) #($a $b) #($b $c $d) #($a $b $d $e))
		do: [ :c | self assert: (circuits anySatisfy: [ :ci | c hasEqualElements: ci ]) ]
]

{ #category : #'tests-snd' }
MalCyclesCoverageTest >> testAseSccWithSnd [
	| sed circuits |
	sed := MalSccNodeDecomposition new.
	MalGraphFixture aseSccGraph: sed.
	sed run.
	self assert: 4 equals: sed circuits size.
	circuits := sed circuits asOrderedCollection.
	#(#($b $d) #($a $b) #($b $c $d) #($a $b $d $e))
		do: [ :c | self assert: (circuits anySatisfy: [ :ci | c hasEqualElements: ci ]) ]
]

{ #category : #testing }
MalCyclesCoverageTest >> testNormalize [
	self assert: (#($a $b) hasEqualElements: (cc normalize: #($a $b) asOrderedCollection)).
	self assert: (#($a $b) hasEqualElements: (cc normalize: #($b $a) asOrderedCollection)).
	self assert: (#($b $c $d) hasEqualElements: (cc normalize: #($c $d $b) asOrderedCollection)).
	self assert: (#($a $e $b $f $d) hasEqualElements: (cc normalize: #($e $b $f $d $a) asOrderedCollection))
]
