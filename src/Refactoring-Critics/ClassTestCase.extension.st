Extension { #name : #ClassTestCase }

{ #category : #'*Refactoring-Critics' }
ClassTestCase >> testMethodsOfTheClassShouldNotBeRepeatedInItsSuperclasses [
		
	self assertValidLintRule: RBEquivalentSuperclassMethodsRule new
]

{ #category : #'*Refactoring-Critics' }
ClassTestCase >> testTraitExplicitRequirementMethodsMustBeImplementedInTheClassOrInASuperclass [

	self assertValidLintRule: RBExplicitRequirementMethodsRule new
]
