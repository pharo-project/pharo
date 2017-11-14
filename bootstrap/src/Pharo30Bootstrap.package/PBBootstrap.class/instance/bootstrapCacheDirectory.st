accessing
bootstrapCacheDirectory
	
	^ (Smalltalk os environment at: 'BOOTSTRAP_CACHE' ifAbsent: ['bootstrap-cache']) asFileReference ensureCreateDirectory