querying
versionsChangedSince: anotherCommit
	| changes | 	
	changes := OrderedCollection new.
	self repository loadedPackages do: [ :package | 
		| myVersion theirVersion |
		myVersion := self versionFor: package.
		theirVersion := anotherCommit versionFor: package.
		(myVersion = theirVersion)
			ifFalse: (changes add: myVersion)
	].
	
	^ changes
