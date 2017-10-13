tests
testPragmaMethod
	| exportedClass aMethod |
	
	exportedClass := HEClass for: HETeSimpleClass. 
	self changeNameOf: exportedClass to: #HETeSimpleClass2.
	
	aClass := builder build: exportedClass.
	builder installClass: aClass in: exportedClass category.
	builder installMethods: exportedClass into: aClass.
	
	aMethod := aClass >> #pragmaMethod.
	self assert: aMethod pragmas first method == aMethod. 