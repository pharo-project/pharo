visiting
visitCascadeNode: aCascadeNode 
	aCascadeNode messages do: [:each | self visitNode: each]