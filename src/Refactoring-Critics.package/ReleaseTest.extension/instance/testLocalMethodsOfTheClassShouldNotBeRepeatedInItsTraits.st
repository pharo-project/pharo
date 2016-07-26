*Refactoring-Critics
testLocalMethodsOfTheClassShouldNotBeRepeatedInItsTraits
	"If a class is using a trait with an explicit requirement method, this class should implement the method"
	
	self assertValidLintRule: RBLocalMethodsOfAClassNotInItsTraitComposition new