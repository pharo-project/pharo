development support
repositoryFor: pkgSpec with: gofer

	^([self resolveSpec: pkgSpec with: gofer]
		on: Error
		do: [:ignored | ^nil ]) repository