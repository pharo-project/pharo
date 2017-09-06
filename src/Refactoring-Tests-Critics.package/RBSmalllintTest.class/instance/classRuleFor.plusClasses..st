private
classRuleFor: aTestSelector plusClasses: aCollectionOfTestedClasses
	| result expectedResultEnvironment |
	result := self
		runRuleFor: aTestSelector
		onEnvironment: self classEnvironmentWithAllObjectClasses.
	expectedResultEnvironment :=
		self classEnvironmentWithAll:
			(aCollectionOfTestedClasses copyWith:
				(self objectClassFor: aTestSelector)).
	self
		assertResultOfRule: result
		hasSameClassesAsEnvironment: expectedResultEnvironment