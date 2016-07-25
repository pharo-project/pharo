instance creation
newWithPackage: aPackageName
	| repoManager |
	
	repoManager := self new.
	aPackageName ifNotNil: [ repoManager 
								packageName: aPackageName;
								initializeSelectedRemotes;
								yourself ].
	^ repoManager