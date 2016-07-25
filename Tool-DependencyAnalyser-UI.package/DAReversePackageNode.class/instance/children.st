accessing
children
	^ (self content usersOfPackage 
		collect: [ :each | each asPackageDependencyWrapper ]) asSortedCollection