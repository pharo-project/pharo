Extension { #name : #RGTraitDefinitionTest }

{ #category : #'*Ring-Tests-Monticello' }
RGTraitDefinitionTest >> testAsFullTraitDefinition [
	
	| rgClass |
	rgClass := TBehavior asRingDefinition.
	self assert: rgClass methods isEmpty.
	self assert: rgClass superclass isNil.
	self assert: rgClass subclasses isEmpty.
	self assert: rgClass package isNil.

	rgClass := TBehavior asFullRingDefinition.
	self assert: rgClass methods notEmpty.
	self assert: (rgClass methodNamed: #allSelectors) package notNil.
	self assert: rgClass superclass notNil.
	self assert: rgClass superclass name = #Trait.
	self assert: rgClass subclasses isEmpty.
	self assert: rgClass package notNil.
	self assert: rgClass package = rgClass theNonMetaClass package.
	self assert: rgClass package name = #Traits.
	self assert: rgClass category = #'Traits-Kernel-Traits'.
	
	self assert: rgClass superclass superclass isNil.
	self assert: rgClass superclass package name equals: #Traits.
	
	rgClass := TBehavior theMetaClass asFullRingDefinition.
	self assert: rgClass package = rgClass theNonMetaClass package.
]
