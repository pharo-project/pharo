preparation
exportMonticelloInStFile

	self
		exportPackages: #('System-Model' 'Ring-Deprecated-ChunkImporter' 'Zinc-Resource-Meta-Core' 'System-Changes' 'Ring-Deprecated-Core-Kernel' 'Ring-Deprecated-Core-Containers' 'Ring-Deprecated-ChunkImporter'  'Compression' 'Monticello' 'Ring-Deprecated-Monticello' )
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
