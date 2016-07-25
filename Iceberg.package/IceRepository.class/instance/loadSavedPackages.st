private
loadSavedPackages
	^ savedPackages := (self allPackages collect: [ :pkg | pkg packageName -> 
		(IceSavedPackage new 
			package: pkg; 
			repository: self; 
			yourself) 
		]) asDictionary