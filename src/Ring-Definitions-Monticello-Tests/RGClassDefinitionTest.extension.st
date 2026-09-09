Extension { #name : 'RGClassDefinitionTest' }

{ #category : '*Ring-Definitions-Monticello-Tests' }
RGClassDefinitionTest >> testAsFullClassDefinition [

	| rgClass |
	rgClass := RGClassDefinitionTest asRingDefinition.
	self assertEmpty: rgClass methods.
	self assert: rgClass superclass isNil.
	self assertEmpty: rgClass subclasses.
	self assert: rgClass package name equals: #'Ring-Definitions-Core-Tests'.

	rgClass := RGClassDefinitionTest asFullRingDefinition.
	self denyEmpty: rgClass methods.
	self assert: (rgClass methodNamed: #testAsFullClassDefinition) package name equals: #'Ring-Definitions-Monticello-Tests'.
	self assert: rgClass superclass isNotNil.
	self assert: rgClass superclass name equals: #TestCase.
	self assertEmpty: rgClass subclasses.
	self assert: rgClass package isNotNil.
	self assert: rgClass package equals: rgClass instanceSide package.
	self assert: rgClass package name equals: #'Ring-Definitions-Core-Tests'.
	self assert: rgClass packageTag equals: #Base.
	self denyEmpty: rgClass extensionMethods.

	self assert: rgClass superclass superclass isNil.
	self assert: rgClass superclass package name equals: #'SUnit-Core'.

	rgClass := RGClassDefinition classSide asFullRingDefinition.
	self assert: rgClass package equals: rgClass instanceSide package
]
