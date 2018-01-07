preparation
exportMonticelloInStFile

	self
		exportPackages: #( 'Ring2-Core' 'Ring2-Compatibility' 'Ring2-ChunkImporter' 'Ring2-Monticello' 'Zinc-Resource-Meta-Core' 'System-Changes' 'Compression' 'Monticello' )
		usingInitializeScript: '
		ChangeSet initialize.
		
		ZipConstants initialize.
		ZipFileConstants initialize.
		ZipWriteStream initialize.
		GZipConstants initialize.
		InflateStream initialize.
		FastInflateStream initialize.
		
		MCCacheRepository initialize.
		MCPackageManager initialize.
		MCLazyVersionInfo initialize.
		MCMethodDefinition initialize.
		MCDataStream initialize.
		MCWorkingCopy initialize.'
		intoFile: 'Monticello.st'
