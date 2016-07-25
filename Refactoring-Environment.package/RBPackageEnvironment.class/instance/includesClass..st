testing
includesClass: aClass
	^ (super includesClass: aClass) and: [ 
		self packages anySatisfy: [ :package | 
			(package includesClass: aClass) or: [ 
			(package extensionCategoriesForClass: aClass) notEmpty ]]]