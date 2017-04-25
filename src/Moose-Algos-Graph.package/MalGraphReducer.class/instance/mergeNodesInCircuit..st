merging
mergeNodesInCircuit: aNodeList
	| sortedNodes |
	"ensure that we get the same order in nodes part of a cycle. MalTarjan uses a dictionary and order depends on model hash that can differ from a machine to another one."
	sortedNodes := aNodeList sort: [ :one :other | one model < other model ].
	circuitNodes add: (self nodeClass new
		mergedNodes: sortedNodes;
		model: (sortedNodes collect: #model);
		nextNodes: (self nextNodesFor: sortedNodes);
		previousNodes: (self previousNodesFor: sortedNodes);
		yourself)