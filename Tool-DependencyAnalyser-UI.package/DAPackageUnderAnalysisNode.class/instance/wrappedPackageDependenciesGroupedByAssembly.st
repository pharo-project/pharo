dependencies
wrappedPackageDependenciesGroupedByAssembly
	| assemblyNodes |
	assemblyNodes := self assemblyNodesFor: { CollectionsAssembly . PharoKernelAssembly }.
	^ ((self packageDependencies
		reject: [ :dependency | 
			assemblyNodes anySatisfy: [ :assemblyNode | assemblyNode includes: dependency packageName ] ]
		thenCollect: #asPackageDependencyWrapper) sorted asOrderedCollection)
		addAll: assemblyNodes;
		yourself