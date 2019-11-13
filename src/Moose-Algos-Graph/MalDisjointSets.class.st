Class {
	#name : #MalDisjointSets,
	#superclass : #MalGraphAlgorithm,
	#instVars : [
		'components'
	],
	#category : #'Moose-Algos-Graph'
}

{ #category : #accessing }
MalDisjointSets >> components [

	^ (components keys collect: [:parent| 
		(components at: parent) collect: [:n| n model] ]) asOrderedCollection 
]

{ #category : #configuration }
MalDisjointSets >> nodeClass [

	^ MalDisjointSetsNextNode 
]

{ #category : #running }
MalDisjointSets >> run [

	"partition nodes by connectivity"
	self nodes do: [:n|
		n nextNodes do: [:next | 
			n union: next ]].
	components := Dictionary new.
	self nodes inject: components into: [:d :n |
		(d at: n find ifAbsentPut: [OrderedCollection new]) add: n. d ]
]
