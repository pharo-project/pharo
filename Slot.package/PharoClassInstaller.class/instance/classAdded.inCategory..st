notifications
classAdded: aClass inCategory: aCategory
	" Install the new class in the system "
	environment at: aClass name put: aClass.
	environment flushClassNameCache.
	
	" Update the system's organization "
	environment organization ifNotNil: [:org | org classify: aClass name under: aCategory].
	aClass environment: environment.
	
	self systemAnnouncer classAdded: aClass inCategory: aCategory.
	" Inform superclass of new subclass "
	aClass superclass addSubclass: aClass.