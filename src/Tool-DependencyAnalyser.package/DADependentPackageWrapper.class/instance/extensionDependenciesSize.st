accessing
extensionDependenciesSize
	| usersOf |
	usersOf := self usersOfPackage.
	^ usersOf 
			inject: 0 
			into: [ :sum :each | sum + (each extensionDependenciesSizeTo: self daPackage) ]