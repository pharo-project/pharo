build items
buildNodes
	relationGraph seenPackages do: [ :package | graphviz add: package packageName ] displayingProgress: 'Building nodes'