accessing
fetchingSpecLoader

	^(MetacelloFetchingMCSpecLoader on: self spec)
		shouldDisablePackageCache: self shouldDisablePackageCache;
		loaderPolicy: self loaderPolicy copy;
		yourself