accessing
loadPackageList

	| vrsn pkgs |
	(vrsn := self versionOrNil) == nil ifTrue: [ ^#() ].
	pkgs := OrderedCollection new.
	(self loadListForVersion: vrsn) do: [:nm |
		pkgs addAll: ((vrsn packagesForSpecNamed: nm ) collect: [:each | each name ])].
	^pkgs