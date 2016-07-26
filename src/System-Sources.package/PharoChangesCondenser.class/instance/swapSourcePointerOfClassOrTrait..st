private - 2 swapping
swapSourcePointerOfClassOrTrait: classOrTrait

	self swapClassComment: classOrTrait.
	
	classOrTrait methodsDo: [ :method | 
		self swapSourcePointerOfMethod: method ].
	
	classOrTrait classSide methodsDo: [ :method | 
		self swapSourcePointerOfMethod: method ]