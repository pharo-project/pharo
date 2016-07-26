*rpackage-core
extensionSelectors
	^ self extendingPackages flatCollect: [ :each | each extensionSelectorsForClass: self ]