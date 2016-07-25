testing
packageVisited: aMTPackage
	^ packages anySatisfy: [ :each | each name = aMTPackage name ]