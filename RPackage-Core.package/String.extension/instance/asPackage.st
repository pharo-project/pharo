*RPackage-Core
asPackage
	^ self asPackageIfAbsent: [ NotFound new messageText: 'There is no package named ', self; object: self; signal ] 