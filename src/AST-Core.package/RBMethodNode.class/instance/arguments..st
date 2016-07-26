accessing
arguments: variableNodes 
	arguments := variableNodes.
	arguments do: [:each | each parent: self]