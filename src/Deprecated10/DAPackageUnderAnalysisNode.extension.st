Extension { #name : #DAPackageUnderAnalysisNode }

{ #category : #'*Deprecated10' }
DAPackageUnderAnalysisNode >> wrappedPackageDependenciesGroupedByAssembly [
	| assemblyNodes |
	self deprecated: 'Deprecated together with assembly mechanism. See issue #9670'.
	assemblyNodes := self assemblyNodesFor: { CollectionsAssembly . PharoKernelAssembly }.
	^ ((self packageDependencies
		reject: [ :dependency | 
			assemblyNodes anySatisfy: [ :assemblyNode | assemblyNode includes: dependency packageName ] ]
		thenCollect: #asPackageDependencyWrapper) sorted asOrderedCollection)
		addAll: assemblyNodes;
		yourself
]
