Extension { #name : #ReleaseTest }

{ #category : #'*Refactoring-Critics' }
ReleaseTest >> testExplicitRequirementMethodsShouldBeImplementedInTheirUsers [
	"If a class is using a trait with an explicit requirement method, this class should implement the method"
	
	self assertValidLintRule: RBExplicitRequirementMethodsRule new
	
]

{ #category : #'*Refactoring-Critics' }
ReleaseTest >> testLocalMethodsOfTheClassShouldNotBeRepeatedInItsTraits [
	"If a class is using a trait with an explicit requirement method, this class should implement the method"
	
	self assertValidLintRule: RBLocalMethodsOfAClassNotInItsTraitComposition new
]
