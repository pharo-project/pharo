initialization
fillFor: aClass
	self
		superclass: aClass superclass;
		name: aClass name;
		layoutClass: aClass classLayout class;
		slots: aClass slots ;
		sharedVariablesFromString: aClass classVariablesString;
		sharedPools: aClass sharedPoolsString;
		category: aClass category;
		environment: aClass environment;
		copyClassSlotsFromExistingClass