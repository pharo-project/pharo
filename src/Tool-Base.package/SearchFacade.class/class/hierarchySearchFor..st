class search
hierarchySearchFor: aClass
	| classes |
	classes := aClass allSuperclasses reverse, (aClass allSubclasses sorted: [ :a :b| a name <= b name ]).
	^ self classSearchIn: classes