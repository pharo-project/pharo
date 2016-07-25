accessing
dependenciesSize
	| usersOf |
	usersOf := self usersOfPackage.
	^ usersOf
			 inject: 0
			 into: [ :sum :each | sum + (each dependenciesSizeTo: self daPackage) ]