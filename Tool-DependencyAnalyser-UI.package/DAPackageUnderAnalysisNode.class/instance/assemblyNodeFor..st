dependencies
assemblyNodeFor: anAssemblyManifest
	^ (DAAssemblyNode newWith: (self packageDependencies 
			select: [ :dependency | anAssemblyManifest includes: dependency packageName ]
			thenCollect: #asPackageDependencyWrapper))
		content: anAssemblyManifest packageName;
		yourself