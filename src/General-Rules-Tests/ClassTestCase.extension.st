Extension { #name : 'ClassTestCase' }

{ #category : '*General-Rules-Tests' }
ClassTestCase >> testMethodsOfTheClassShouldNotBeRepeatedInItsSuperclasses [

	self assertValidLintRule: ReEquivalentSuperclassMethodsRule new
]

{ #category : '*General-Rules-Tests' }
ClassTestCase >> testTraitExplicitRequirementMethodsMustBeImplementedInTheClassOrInASuperclass [

	self assertValidLintRule: ReExplicitRequirementMethodsRule new
]
