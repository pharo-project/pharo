updating
updateReferencesTo: aNodeList by: anotherNode
	aNodeList do: 
		[ :mergedNode | 
		self 
			updatePreviousNodesOf: mergedNode
			replacing: aNodeList
			by: anotherNode.
		self 
			updateNextNodesOf: mergedNode
			replacing: aNodeList
			by: anotherNode ]