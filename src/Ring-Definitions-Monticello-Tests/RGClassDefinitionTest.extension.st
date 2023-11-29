Extension { #name : 'RGClassDefinitionTest' }

{ #category : '*Ring-Definitions-Monticello-Tests' }
RGClassDefinitionTest >> testAsFullClassDefinition [

	| rgClass |
	rgClass := Class asRingDefinition.
	self assertEmpty: rgClass methods.
	self assert: rgClass superclass isNil.
	self assertEmpty: rgClass subclasses.
	self assert: rgClass package name equals: #Kernel.

	rgClass := Class asFullRingDefinition.
	self denyEmpty: rgClass methods.
	self assert: (rgClass methodNamed: #asRingDefinition) package name equals: #'Ring-Definitions-Core'.
	self assert: rgClass superclass notNil.
	self assert: rgClass superclass name equals: #ClassDescription.
	self denyEmpty: rgClass subclasses.
	self assert: rgClass package notNil.
	self assert: rgClass package equals: rgClass instanceSide package.
	self assert: rgClass package name equals: #Kernel.
	self assert: rgClass packageTag equals: #Classes.
	self denyEmpty: rgClass extensionMethods.

	self assert: rgClass superclass superclass isNil.
	self assert: rgClass superclass package name equals: #Kernel.
	self assert: rgClass subclasses first package name equals: #Kernel.

	rgClass := RGClassDefinition classSide asFullRingDefinition.
	self assert: rgClass package equals: rgClass instanceSide package
]
