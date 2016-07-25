asserting
assertSelectedCompiledMethod
	| aText |
	self 
		assert: nautilus selectedPackage package 
			equals: compiledMethod package;
		assert: nautilus selectedPackage item categoryName 
			equals: compiledMethod methodClass category;
		assert: nautilus selectedClass
			equals: compiledMethod methodClass;
		assert: nautilus selectedCategory 
			equals: expectedMethodCategory;
		assert: nautilus selectedMethod 
			equals: expectedCompiledMethod;
		assert: nautilus ui packageWidget selectedPackage package 
			equals: compiledMethod package;
		assert: nautilus ui packageWidget selectedPackage item categoryName 
			equals: compiledMethod methodClass category;
		assert: nautilus ui classWidget selectedClass 
			equals: compiledMethod methodClass;
		assert: nautilus ui methodWidget selectedMethods asArray 
			equals: ((Array with: expectedCompiledMethod) copyWithout: nil).

	aText := nautilus ui sourceCodeContainer submorphs first submorphs first text asString.
	nautilus selectedMethod 
		ifNotNil: [ self assert: aText equals: compiledMethod sourceCode ].
		
	self assertPackageWidget.
	self assertMethodWidget.