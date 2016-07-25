testing
includesClass: aClass
	^ aClass == class and: [super includesClass: aClass]