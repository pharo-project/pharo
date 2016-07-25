converting
promoteAsRPackage
	"This method converts this rpackage tag into an rpackage, removes the tag from the parent package with all classes included and registers the new package in the system.
	The tag has to be removed before registering to avoid conflicts.
	Smells like we could have an error and lose package tags! registerPackage should not fail because names Package-Tag are unique in the system."
	| newRPackage |
	
	newRPackage := self asRPackage.
	self classes do: [ :each | self package removeClass: each  ].
	self package removeClassTag: self name.
	self package removeMethods: newRPackage extensionMethods.
	RPackage organizer registerPackage: newRPackage.
	
	newRPackage classes do: [ :each | 
		SystemAnnouncer uniqueInstance 
			classRepackaged: each 
			from: self package
			to:  newRPackage ].