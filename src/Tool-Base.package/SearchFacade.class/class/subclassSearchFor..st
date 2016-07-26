class search
subclassSearchFor: aClass
	| classes |
	classes := aClass allSubclasses sorted: [ :a :b| a name <= b name ].
	^ self classSearchIn: classes