removing
removePackage: package

	(packages 
		removeKey: package package name
		ifAbsent: [ ^ self ]) flush