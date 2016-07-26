repository updating
updatePackageRepositoriesFor: versionString

	| versionSpec |
	(versionSpec := (self version: versionString) versionSpec) packageSpecsInLoadOrder do: [:pkgSpec |
		pkgSpec updatePackageRepositoriesFor: versionSpec ].
	^true