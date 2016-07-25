development support
goferCommitProject: commitMessage

	| pkgSpec |
	(pkgSpec := self projectPackage) == nil ifTrue: [ ^false ].
	^pkgSpec goferCommitPackage: commitMessage