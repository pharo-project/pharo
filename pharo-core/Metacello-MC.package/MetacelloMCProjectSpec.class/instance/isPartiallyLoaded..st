testing
isPartiallyLoaded: aLoader

	| vrsn |
	(vrsn := self versionOrNil) == nil ifTrue: [ ^false ].
	(self loadListForVersion: vrsn) do: [:nm |
		(vrsn packagesForSpecNamed: nm ) do: [:pkg |
			(pkg isPackageLoaded: aLoader) ifTrue: [ ^true ]]].
	^false