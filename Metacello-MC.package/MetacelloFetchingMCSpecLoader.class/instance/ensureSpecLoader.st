accessing
ensureSpecLoader
	^ (MetacelloEnsureFetchingMCSpecLoader on: self spec)
		shouldDisablePackageCache: self shouldDisablePackageCache;
		loaderPolicy: self loaderPolicy; "explicitly share the loaderPolicy"
		yourself