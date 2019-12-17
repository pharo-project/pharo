Extension { #name : #RGTraitDefinitionTest }

{ #category : #'*Ring-Definitions-Tests-Monticello' }
RGTraitDefinitionTest >> testAsFullTraitDefinition [
	| rgClass |
	rgClass := TSortable asRingDefinition.
	self assertEmpty: rgClass methods.
	self assert: rgClass superclass isNil.
	self assertEmpty: rgClass subclasses.
	self assert: rgClass package isNil.

	rgClass := TSortable asFullRingDefinition.
	self assert: rgClass methods notEmpty.
	self assert: (rgClass methodNamed: #isSorted) package notNil.
	self assert: rgClass superclass notNil.
	self assert: rgClass superclass name equals: #Trait.
	self assertEmpty: rgClass subclasses.
	self assert: rgClass package notNil.
	self assert: rgClass package equals: rgClass instanceSide package.
	self assert: rgClass package name equals: #'Collections-Abstract-Traits'.
	self assert: rgClass category equals: #'Collections-Abstract-Traits'.

	self assert: rgClass superclass superclass isNil.
	self assert: rgClass superclass package name equals: #TraitsV2.

	rgClass := TBehavior classSide asFullRingDefinition.
	self assert: rgClass package equals: rgClass classSide package
]
