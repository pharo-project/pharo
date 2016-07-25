manifest
createManifestNamed: packageName

	 manifestClass := PackageManifest subclass: (self class manifestTag, packageName onlyLetters) asSymbol
			instanceVariableNames: ''
			classVariableNames: ''
			poolDictionaries: ''
			category: packageName.
	manifestClass comment: self class manifestClassComment.
	^ manifestClass