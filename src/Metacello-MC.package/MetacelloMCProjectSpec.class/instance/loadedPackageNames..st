accessing
loadedPackageNames: aLoader

	| vrsn pkgs |
	(vrsn := self versionOrNil) == nil ifTrue: [ ^#() ].
	pkgs := OrderedCollection new.
	(self loadListForVersion: vrsn) do: [:nm |
		(vrsn packagesForSpecNamed: nm ) do: [:pkg |
			(pkg isPackageLoaded: aLoader) ifTrue: [ pkgs add: pkg name ]]].
	^pkgs