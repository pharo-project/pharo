Extension { #name : #RGTraitDefinitionTest }

{ #category : #'*Ring-Tests-Monticello' }
RGTraitDefinitionTest >> testAsFullTraitDefinition [
	
	| rgClass |
	rgClass := TSortable asRingDefinition.
	self assert: rgClass methods isEmpty.
	self assert: rgClass superclass isNil.
	self assert: rgClass subclasses isEmpty.
	self assert: rgClass package isNil.

	rgClass := TSortable asFullRingDefinition.
	self assert: rgClass methods notEmpty.
	self assert: (rgClass methodNamed: #isSorted) package notNil.
	self assert: rgClass superclass notNil.
	self assert: rgClass superclass name = #Trait.
	self assert: rgClass subclasses isEmpty.
	self assert: rgClass package notNil.
	self assert: rgClass package = rgClass instanceSide package.
	self assert: rgClass package name = #'Collections-Abstract-Traits'.
	self assert: rgClass category = #'Collections-Abstract-Traits'.
	
	self assert: rgClass superclass superclass isNil.
	self assert: rgClass superclass package name equals: #TraitsV2.
	
	rgClass := TBehavior classSide asFullRingDefinition.
	self assert: rgClass package = rgClass classSide package.
]
