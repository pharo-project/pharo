testing
includesClass: aClass 
	^(self privateSelectorsForClass: aClass) isEmpty not 
		and: [super includesClass: aClass]