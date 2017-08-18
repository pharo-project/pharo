tests
testCreatingAClassUsingComplexTraits
	| exportedClass obj|
	
	exportedClass := HEClass for: HETeSimpleClassWithComplexTraits. 
	self changeNameOf: exportedClass to: #HETeSimpleClassWithComplexTraitsNew.
	
	aClass := builder build: exportedClass.
	
	self assert: aClass name equals: #HETeSimpleClassWithComplexTraitsNew.
	self assert: aClass class name equals: #'HETeSimpleClassWithComplexTraitsNew class'.

	builder installClass: aClass in: exportedClass category.
	builder installMethods: exportedClass into: aClass.

	self deny: aClass traitComposition isEmpty.
	self deny: aClass class traitComposition isEmpty.
		
	obj := aClass new.
	
	self assert: obj oneMessage equals: 1234.
	self assert: obj otherMessage equals: 4321.	