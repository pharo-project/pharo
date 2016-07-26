accessing
packages
	^ self rootEnvironment packages 
		select: [ :package | self includesPackage: package ]