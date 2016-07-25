build items
buildEdges
	relationGraph packages
		do: [ :package | 
			package dependencies
				do: [ :dependency | 
					graphviz
						add: dependency source packageName -> dependency target packageName
						with: {(#label -> dependency numberOfDependencies)} ] ]
		displayingProgress: 'Building edges'