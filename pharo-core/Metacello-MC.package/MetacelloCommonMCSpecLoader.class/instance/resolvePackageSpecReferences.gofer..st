private
resolvePackageSpecReferences: packageSpec gofer: gofer

	|  versionReference references localGofer |
	localGofer := gofer.
	self hasRepositoryOverrides not
		ifTrue: [
			packageSpec repositorySpecs notEmpty
				ifTrue: [ 
					localGofer := MetacelloGofer new.
					(self repositoriesFrom: packageSpec repositorySpecs) do: [:repo | localGofer repository: repo ]]].
	(packageSpec getFile == nil or: [ self shouldDisablePackageCache ])
		ifTrue: [ 
			"don't use package-cache when trying to get latest version"
			localGofer disablePackageCache ].
	versionReference := packageSpec goferLoaderReference.
	references := versionReference resolveAllWith: localGofer.
	localGofer enablePackageCache.
	^references