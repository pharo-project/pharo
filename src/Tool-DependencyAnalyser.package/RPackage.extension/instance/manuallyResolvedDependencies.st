*Tool-DependencyAnalyser
manuallyResolvedDependencies

	^ self packageManifestOrNil
		ifNil: [ #() ]
		ifNotNil: #manuallyResolvedDependencies