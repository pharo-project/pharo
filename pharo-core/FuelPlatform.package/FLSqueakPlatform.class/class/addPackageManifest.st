private-hacks
addPackageManifest
	Smalltalk
		at: #PackageManifest
		ifPresent: [ :manifest | ^ self ].
	
	Object subclass: #PackageManifest
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: self hacksCategoryName