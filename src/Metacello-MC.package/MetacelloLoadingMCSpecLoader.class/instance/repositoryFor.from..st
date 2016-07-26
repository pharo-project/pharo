development support
repositoryFor: pkgSpec from: repositorySpecs

	^([self resolveSpec: pkgSpec from: repositorySpecs]
		on: Error
		do: [:ignored | ^nil ]) repository