Extension { #name : #ClassTestCase }

{ #category : #'*Refactoring-Critics' }
ClassTestCase >> testMethodsOfTheClassShouldNotBeRepeatedInItsSuperclasses [
		
	self assertValidLintRule: ReEquivalentSuperclassMethodsRule new
]

{ #category : #'*Refactoring-Critics' }
ClassTestCase >> testTraitExplicitRequirementMethodsMustBeImplementedInTheClassOrInASuperclass [

	self assertValidLintRule: ReExplicitRequirementMethodsRule new
]
