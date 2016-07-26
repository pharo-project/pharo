instance creation
loadPackage: aPackageSpec externalReference: externalReference loader: aLoader

	^MetacelloPackageLoadDirective new
		spec: aPackageSpec externalReference: externalReference  loader: aLoader