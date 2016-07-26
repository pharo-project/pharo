updating
removeReferencesToMergedNodes
	circuitNodes do: 
		[ :circuitNode |
		self 
			updateReferencesTo: circuitNode mergedNodes
			by: circuitNode.
		self removeCircularDependenciesFrom: circuitNode ]