loading
fetchPackage: aLoaderPolicy

	self fetchUsing: 
		(self loader
			loaderPolicy: aLoaderPolicy;
			yourself)