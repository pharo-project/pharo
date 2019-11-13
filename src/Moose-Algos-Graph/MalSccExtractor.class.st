"
A utility class to easily get circuits, i.e. SCC with more than one node.
"
Class {
	#name : #MalSccExtractor,
	#superclass : #MalGraphAlgorithm,
	#instVars : [
		'circuits'
	],
	#category : #'Moose-Algos-Graph'
}

{ #category : #accessing }
MalSccExtractor >> addCircuit: aCollectionOfNodes [
	
	circuits add: aCollectionOfNodes
]

{ #category : #accessing }
MalSccExtractor >> circuits [
	^ circuits
]

{ #category : #initialization }
MalSccExtractor >> initialize [ 

	super initialize.
	circuits := Set new.

]
