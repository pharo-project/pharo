protocol
buildRootForPackage: aPackage

	^ KomitPackageNode new
		content: aPackage;
		yourself