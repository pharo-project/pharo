preparation
exportMonticelloInStFile

	self
		exportPackages: #( 'Ring2-Core' 'Ring2-ChunkImporter'  'Zinc-Resource-Meta-Core' 'System-Changes' 'Compression' 'Ring2-Compatibility' 'Monticello' 'Ring2-Monticello' )
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
