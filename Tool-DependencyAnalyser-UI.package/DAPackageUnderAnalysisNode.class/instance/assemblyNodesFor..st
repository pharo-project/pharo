dependencies
assemblyNodesFor: aListOfAssemblyManifest
	^ aListOfAssemblyManifest 
		collect: [ :each | self assemblyNodeFor: each ]
		as: OrderedCollection