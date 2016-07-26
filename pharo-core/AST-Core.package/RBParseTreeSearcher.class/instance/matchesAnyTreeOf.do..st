searching
matchesAnyTreeOf: treeCollection do: aBlock 
	treeCollection do: [:each | self matchesTree: each do: aBlock]