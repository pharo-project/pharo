private
uniqueClassesIn: aBrowserEnvironment 
	| allClasses |
	allClasses := IdentitySet new.
	aBrowserEnvironment classesDo: [ :each | 
		self deny: (allClasses includes: each).
		allClasses add: each ]