testing
addPackageManifest
	Smalltalk
		at: #PackageManifest
		ifPresent: [ :x | ^ self ].
	
	Object subclass: #PackageManifest
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: self hacksCategoryName