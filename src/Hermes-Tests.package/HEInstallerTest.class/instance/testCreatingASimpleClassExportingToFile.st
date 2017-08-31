tests
testCreatingASimpleClassExportingToFile
	| exportedClass obj readedClass |
	
	exportedClass := HEClass for: HETeSimpleClass. 
	self changeNameOf: exportedClass to: #HETeSimpleClassNew.

	exportedClass writeInto: writer.
	writer flush.
	
	readedClass := HEClass readFrom: reader.

	self assert: readedClass equals: exportedClass.
	
	aClass := builder build: exportedClass.	
	builder installClass: aClass in: exportedClass category.
	builder installMethods: exportedClass into: aClass.

	self assert: aClass traitComposition isEmpty.
	self assert: aClass class traitComposition isEmpty.

	obj := aClass newInstance.
	
	obj anInstanceVariable: 25.
	self assert: obj anInstanceVariable equals: 25.
	self assert: obj otherMethod equals: 25.
	self assert: obj usingAConstant equals: 42.