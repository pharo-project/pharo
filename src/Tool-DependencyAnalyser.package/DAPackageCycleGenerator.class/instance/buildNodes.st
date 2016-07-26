build items
buildNodes
	
	packageCycle packages 	
		do: [ :package |
			graphviz add: package packageName.
			 ] displayingProgress: 'Building nodes'