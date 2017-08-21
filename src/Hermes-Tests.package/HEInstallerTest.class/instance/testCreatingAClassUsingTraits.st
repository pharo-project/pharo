tests
testCreatingAClassUsingTraits
	| exportedClass obj|
	
	exportedClass := HEClass for: HETeSimpleClassWithTraits. 
	self changeNameOf: exportedClass to: #HETeSimpleClassWithTraitsNew.
	
	aClass := builder build: exportedClass.
	
	self assert: aClass name equals: #HETeSimpleClassWithTraitsNew.
	self assert: aClass class name equals: #'HETeSimpleClassWithTraitsNew class'.

	builder installClass: aClass in: exportedClass category.
	builder installMethods: exportedClass into: aClass.

	self deny: aClass traitComposition isEmpty.
	self deny: aClass class traitComposition isEmpty.
		
	obj := aClass someClassSideMessage.

	self assert: aClass slots isEmpty.	
	self assert: aClass class slots isEmpty.	

	self assert: obj oneMessage equals: 1234.
	self assert: obj otherMessage equals: 4321.	