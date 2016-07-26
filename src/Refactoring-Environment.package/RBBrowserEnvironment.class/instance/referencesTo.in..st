environments
referencesTo: aLiteral in: aClass 
	| classes |
	classes := aClass withAllSuperclasses asSet.
	classes
		addAll: aClass allSubclasses;
		addAll: aClass class withAllSuperclasses;
		addAll: aClass class allSubclasses.
	^(self forClasses: classes) referencesTo: aLiteral