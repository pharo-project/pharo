Extension { #name : #ClassTestCase }

{ #category : #'*Refactoring-Critics' }
ClassTestCase >> testMethodsOfTheClassShouldNotBeRepeatedInItsSuperclasses [
		
	self assertValidLintRule: RBEquivalentSuperclassMethodsRule new.
	self deliberatelyDidNotAssert
]

{ #category : #'*Refactoring-Critics' }
ClassTestCase >> testTraitExplicitRequirementMethodsMustBeImplementedInTheClassOrInASuperclass [

	self assertValidLintRule: RBExplicitRequirementMethodsRule new.
	
	self deliberatelyDidNotAssert
]
