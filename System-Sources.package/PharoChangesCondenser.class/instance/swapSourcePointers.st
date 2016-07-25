private - 2 swapping
swapSourcePointers
	job 
		title: 'Swapping source pointers';
		currentValue: 0.
	
	Smalltalk allClassesAndTraitsDo: [ :classOrTrait |
		job increment.
		self swapSourcePointerOfClassOrTrait: classOrTrait ]
	