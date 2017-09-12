tests
testUndeclared
	| exportedClass |
	
	UndefinedObject compile:'xxx
		^ AnUndeclaredClass'.
		
	self assert: nil xxx equals: nil.
	
	exportedClass := HEClass for: HETeSimpleClass. 
	self changeNameOf: exportedClass to: #AnUndeclaredClass.

	aClass := builder build: exportedClass.
	builder installClass: aClass in: exportedClass category.
	builder installMethods: exportedClass into: aClass.

	self should: [ Undeclared at: #AnUndeclaredClass ] raise: KeyNotFound.
	Smalltalk at:#AnUndeclaredClass.