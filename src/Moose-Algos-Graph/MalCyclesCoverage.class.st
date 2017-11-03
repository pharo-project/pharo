Class {
	#name : #MalCyclesCoverage,
	#superclass : #MalTarjan,
	#traits : 'MalTNormalize',
	#classTraits : 'MalTNormalize classTrait',
	#instVars : [
		'circuits',
		'sccExtractorClass'
	],
	#category : #Moose-Algos-Graph
}

{ #category : #accessing }
MalCyclesCoverage >> circuits [
	^ circuits
]

{ #category : #running }
MalCyclesCoverage >> directCircuit: aCollection [

	^ self normalize: (OrderedCollection 
		with: aCollection first model 
		with: aCollection second model)
]

{ #category : #initialization }
MalCyclesCoverage >> initialize [

	super initialize.
	self useNodeDecomposition 
]

{ #category : #accessing }
MalCyclesCoverage >> retrieveCircuitsFromSccExtractor: sccExtractor [

	sccExtractor circuits do: [:c |
		circuits add: (c collect: [:n| n model]) ]
]

{ #category : #running }
MalCyclesCoverage >> run [

	super run.
	circuits := OrderedCollection new.
	self stronglyConnectedComponents do: [:scc|
		(scc size > 2) ifTrue: [|sccExtractor|
			sccExtractor := sccExtractorClass new.
			sccExtractor nodes: scc.
			"graph injection remove outgoing edges from SCC"
			sccExtractor edges: scc from: #yourself toAll: #nextNodes.
			sccExtractor run.
			self retrieveCircuitsFromSccExtractor: sccExtractor ].
		(scc size = 2) ifTrue: [
			circuits add: (self directCircuit: scc) ]. "direct circuit"
		]
]

{ #category : #running }
MalCyclesCoverage >> useCircuitSpan [

	sccExtractorClass := MalCircuitSpanningEdges  
]

{ #category : #running }
MalCyclesCoverage >> useEdgeDecomposition [

	sccExtractorClass := MalSccEdgeDecomposition 
]

{ #category : #running }
MalCyclesCoverage >> useNodeDecomposition [

	sccExtractorClass := MalSccNodeDecomposition 
]
