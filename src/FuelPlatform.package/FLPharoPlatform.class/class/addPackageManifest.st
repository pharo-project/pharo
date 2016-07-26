testing
addPackageManifest
	Smalltalk
		at: #PackageManifest
		ifPresent: [ ^ self ].
	
	Object subclass: #PackageManifest
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: self hacksCategoryName