visiting
visitArrayNode: anArrayNode 

	anArrayNode children do: [:each | self visitNode: each]