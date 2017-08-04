preparation
exportMonticelloInStFile

	self
		exportPackages: #( 'RingChunkImporter' 'Zinc-Resource-Meta-Core' 'System-Changes' 'Ring-Core-Kernel' 'Ring-Core-Containers' 'RingChunkImporter'  'Compression' 'Monticello' 'Ring-Monticello' )
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