Class {
	#name : #MalDominance,
	#superclass : #MalGraphAlgorithm,
	#category : #Moose-Algos-Graph
}

{ #category : #configuration }
MalDominance >> nodeClass [

	^ MalDominanceNode
]

{ #category : #running }
MalDominance >> run [

	| source |
	self nodes do: [ :n | n makeSet ].
	self nodes do: [ :n | n hasUniqueSource ifTrue: [ 
								source := n firstSource.
								source flagUnique.
								source union: n ] ]
]
