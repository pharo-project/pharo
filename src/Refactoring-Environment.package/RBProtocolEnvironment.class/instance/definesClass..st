testing
definesClass: aClass
	^ aClass == class and: [super definesClass: aClass]