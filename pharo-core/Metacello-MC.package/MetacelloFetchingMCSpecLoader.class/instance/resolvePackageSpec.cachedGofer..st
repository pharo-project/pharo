private
resolvePackageSpec: packageSpec cachedGofer: gofer

	|  versionReference references |
	versionReference := packageSpec goferLoaderReference.
	(references := versionReference resolveAllWith: gofer) isEmpty 
		ifTrue: [ ^nil ].
	^references last asMetacelloCachingResolvedReference.