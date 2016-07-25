loading
load
	RPackageSet withCacheDo: [   
		| version |
		version := versions first. 
		[ self ensurePackage: version package.
		  self loadWithNameLike: version info name ] asJob
				title: 'Loading ', version info name asString;
				run ]