preparation
exportDeprecatedInStFile

	self
		exportPackages: #( 'Deprecated70' )
		usingInitializeScript: 'MultiByteFileStream initialize'
		intoFile: 'Deprecated70.st'
		