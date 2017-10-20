*Tool-DependencyAnalyser
ignoredDependencies

	^ self packageManifestOrNil
		ifNil: [ #() ]
		ifNotNil: #ignoredDependencies