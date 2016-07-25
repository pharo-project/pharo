class search
classSearchInEnvironment: anEnvironment
	| classes |
	classes :=  anEnvironment classesAndTraits sorted: [ :a :b| a name <= b name ].
	^ self classSearchIn: classes.