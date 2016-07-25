actions
loadWithPolicy: aLoadPolicy

	| gofer |
	gofer := MetacelloGofer new.
	gofer disablePackageCache.
	gofer repository: aLoadPolicy cacheRepository.
	self loadUsing: self gofer: gofer