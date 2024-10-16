Extension { #name : 'RGTraitDefinitionTest' }

{ #category : '*Ring-Definitions-Monticello-Tests' }
RGTraitDefinitionTest >> testAsFullTraitDefinition [

	| rgClass |
	rgClass := TAssertable asRingDefinition.
	self assertEmpty: rgClass methods.
	self assert: rgClass superclass isNil.
	self assertEmpty: rgClass subclasses.
	self assert: rgClass package name equals: #'SUnit-Core-Traits'.

	rgClass := TAssertable asFullRingDefinition.
	self denyEmpty: rgClass methods.
	self assert: (rgClass methodNamed: #assert:equals:) package isNotNil.
	self assert: rgClass superclass isNotNil.
	self assert: rgClass superclass name equals: #Trait.
	self assertEmpty: rgClass subclasses.
	self assert: rgClass package isNotNil.
	self assert: rgClass package equals: rgClass instanceSide package.
	self assert: rgClass package name equals: #'SUnit-Core-Traits'.
	self assert: rgClass packageTag equals: #Uncategorized.

	self assert: rgClass superclass superclass isNil.
	self assert: rgClass superclass package name equals: #Traits.

	rgClass := TEmpty classSide asFullRingDefinition.
	self assert: rgClass package equals: rgClass classSide package
]
