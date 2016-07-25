packages
latestPackage: aString fromRepository: repositorySpecs

	| gofer |
	gofer := MetacelloGofer new.
	gofer disablePackageCache.
	(self repositoriesFrom: repositorySpecs) do: [:repo | gofer repository: repo ].
	^([(GoferPackageReference name: aString) resolveWith: gofer]
		on: Error
		do: [:ignored | ^ nil ]) name