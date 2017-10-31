accessing
dependenciesReport

	^ dependenciesReport ifNil: [ self rebuildDependenciesReport ]
