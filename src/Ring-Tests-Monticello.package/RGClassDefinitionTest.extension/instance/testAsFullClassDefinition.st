*Ring-Tests-Monticello
testAsFullClassDefinition
	"self debug: #testAsFullClassDefinition"
	
	| rgClass |
	rgClass := Class asRingDefinition.
	self assert: rgClass methods isEmpty.
	self assert: rgClass superclass isNil.
	self assert: rgClass subclasses isEmpty.
	self assert: rgClass package name equals: #Kernel.

	rgClass := Class asFullRingDefinition.
	self assert: rgClass methods notEmpty.
	self assert: (rgClass methodNamed: #asRingDefinition) package name = #'Ring-Core-Kernel'.
	self assert: rgClass superclass notNil.
	self assert: rgClass superclass name = #ClassDescription.
	self assert: rgClass subclasses notEmpty.
	self assert: rgClass package notNil.
	self assert: rgClass package = rgClass theNonMetaClass package.
	self assert: rgClass package name = #Kernel.
	self assert: rgClass category = #'Kernel-Classes'.
	self assert: rgClass extensionMethods notEmpty.
	
	self assert: rgClass superclass superclass isNil.
	self assert: rgClass superclass package name equals: #Kernel.
	self assert: rgClass subclasses first package name equals: #Kernel.
	
	rgClass := RGClassDefinition theMetaClass asFullRingDefinition.
	self assert: rgClass package = rgClass theNonMetaClass package.