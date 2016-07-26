development support
resolveSpec: pkgSpec from: repositorySpecs

	| gofer |
	gofer := MetacelloGofer new.
	gofer disablePackageCache.
	(self repositoriesFrom: repositorySpecs ignoreOverrides: true) 
		do: [:repo | gofer repository: repo ].
	^self resolveSpec: pkgSpec with: gofer