Class {
	#name : #MalKruskalTest,
	#superclass : #TestCase,
	#instVars : [
		'nodes',
		'kruskal'
	],
	#category : #'Moose-Algos-Graph-Tests'
}

{ #category : #running }
MalKruskalTest >> setUp [
	super setUp.
	kruskal := MalKruskal new.
	nodes := OrderedCollection new.
	nodes add: MalDisjointSetNode new.
	nodes add: MalDisjointSetNode new.
	nodes add: MalDisjointSetNode new.
	nodes add: MalDisjointSetNode new
]

{ #category : #tests }
MalKruskalTest >> testMaxSpanningTree [
	| tree expectedEdges |
	MalGraphFixture simpleWeightedGraph: kruskal.
	kruskal maxSpanningTree.
	tree := kruskal run collect: [ :e | e asTuple ].

	expectedEdges := #(#(1 2 5) #(3 4 5) #(1 3 4) #(1 5 3)).
	expectedEdges do: [ :e | self assert: (tree includes: e) ].
	tree do: [ :e | self assert: (expectedEdges includes: e) ]
]

{ #category : #tests }
MalKruskalTest >> testMaxSpanningTree2 [
	| tree expectedEdges |
	MalGraphFixture complexWeightedGraph: kruskal.
	kruskal maxSpanningTree.
	tree := kruskal run collect: [ :e | e asTuple ].

	expectedEdges := #(#($a $b 30) #($b $s 1) #($b $c 30) #($d $e 30) #($d $f 20) #($d $j 10) #($e $a 15) #($f $m 8) #($g $h 20) #($g $r 3) #($i $a 14) #($i $k 4) #($j $q 5) #($k $l 10) #($k $g 5) #($m $n 7) #($m $o 6) #($p $b 5)).
	expectedEdges do: [ :e | self assert: (tree includes: e) ].
	tree do: [ :e | self assert: (expectedEdges includes: e) ]
]

{ #category : #tests }
MalKruskalTest >> testMinSpanningTree [
	| tree expectedEdges |
	MalGraphFixture simpleWeightedGraph: kruskal.
	tree := kruskal run collect: [ :e | e asTuple ].

	expectedEdges := #(#(4 5 1) #(5 1 2) #(2 3 2) #(2 4 3)).
	self assert: tree asArray equals: expectedEdges.
	expectedEdges do: [ :e | self assert: (tree includes: e) ].
	tree do: [ :e | self assert: (expectedEdges includes: e) ]
]

{ #category : #tests }
MalKruskalTest >> testMinSpanningTree1 [
	| tree expectedEdges |
	MalGraphFixture complexWeightedGraph: kruskal.
	tree := kruskal run collect: [ :e | e asTuple ].

	expectedEdges := #(#($a $b 30) #($b $s 1) #($b $p 4) #($d $f 20) #($e $a 15) #($f $m 8) #($g $h 20) #($g $r 3) #($i $a 14) #($i $k 4) #($i $d 3) #($j $q 5) #($k $l 10) #($k $g 5) #($m $n 7) #($m $o 6) #($n $c 5) #($q $i 4)).
	"self assert: tree asArray equals: expectedEdges."
	expectedEdges do: [ :e | self assert: (tree includes: e) ].
	tree do: [ :e | self assert: (expectedEdges includes: e) ]
]

{ #category : #tests }
MalKruskalTest >> testUnionFind [
	| node1 node2 node3 node4 |
	nodes
		do: [ :node | 
			self assert: node parent equals: nil.
			node makeSet.
			self assert: node parent equals: node.
			self assert: node find equals: node ].
	node1 := nodes at: 1.
	node2 := nodes at: 2.
	node3 := nodes at: 3.
	node4 := nodes at: 4.
	node1 union: node2.
	self assert: node1 find equals: node1.
	self assert: node2 find equals: node1.
	node2 union: node3.
	self assert: node2 find equals: node1.
	self assert: node3 find equals: node1.
	self assert: node3 parent equals: node1.
	node4 union: node1.
	nodes do: [ :node | self assert: node find equals: node4 ].
	self assert: node1 parent equals: node4.
	self assert: node2 parent equals: node1.
	self assert: node3 parent equals: node1.
	self assert: node4 parent equals: node4
]
