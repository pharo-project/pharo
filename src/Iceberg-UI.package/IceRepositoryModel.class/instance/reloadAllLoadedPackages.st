actions
reloadAllLoadedPackages
	self repository savedPackages 
		select: #isLoaded 
		thenDo: [ :each | each reload ].