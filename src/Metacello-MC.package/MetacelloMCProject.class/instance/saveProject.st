development support
saveProject

	| pkgSpec |
	(pkgSpec := self projectPackage) == nil ifTrue: [ ^false ].
	^pkgSpec savePackage