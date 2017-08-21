running
runPackages: aCollectionOfStrings
	^ aCollectionOfStrings collect: [ :each | self runPackage: each ]