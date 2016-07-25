accessing
definedMethodsForClass: aClass

	^ (self definedSelectorsForClass: aClass) asOrderedCollection collect: [ :each | aClass >> each ]