testing
isAtLeastInCycle: aCollectionOfPDPackage
	^ aCollectionOfPDPackage anySatisfy: [ :package | self isInCycle: package ]