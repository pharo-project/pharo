image package loading
loadPackagesFromImage

	| rgPackage rgPackageKeys |
	rgPackageKeys := OrderedCollection new.
	self class allManagers do: [ :pck | 
		rgPackage := RGPackage named: pck package name asSymbol.
		rgPackageKeys add: (rgPackage name -> ('*', rgPackage name asLowercase)).
		self addPackage: rgPackage ].
	^ rgPackageKeys