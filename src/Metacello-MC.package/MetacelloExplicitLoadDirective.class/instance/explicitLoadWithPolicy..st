actions
explicitLoadWithPolicy: aLoadPolicy

	| gofer |
	gofer := MetacelloGofer new.
	gofer disablePackageCache.
	gofer repository: aLoadPolicy cacheRepository.
	self explicitLoadUsing: self gofer: gofer