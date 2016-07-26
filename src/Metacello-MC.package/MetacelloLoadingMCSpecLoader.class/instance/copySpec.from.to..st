development support
copySpec: pkgSpec from: repositorySpecs to: repository

	| gofer |
	gofer := MetacelloGofer new.
	(self repositoriesFrom: repositorySpecs) do: [:repo | gofer repository: repo ].
	^self copySpec: pkgSpec with: gofer to: repository