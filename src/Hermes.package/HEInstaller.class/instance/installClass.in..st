creating classes
installClass: aClass in: category
	environment at: aClass name put: aClass.
	aClass superclass addSubclass: aClass.

	aClass basicCategory: category.
   environment organization ifNotNil: [ :e | e classify: aClass name under: category ].
	SystemAnnouncer uniqueInstance classAdded: aClass inCategory: category.
