private
classRuleFor: aTestSelector plusClasses: aCollectionOfTestedClasses
	| rule expectedResultEnvironment |
	rule := self
		runRuleFor: aTestSelector
		onEnvironment: self classEnvironmentWithAllObjectClasses.
	expectedResultEnvironment :=
		self classEnvironmentWithAll:
			(aCollectionOfTestedClasses copyWith:
				(self objectClassFor: aTestSelector)).
	self
		assertResultOfRule: rule
		hasSameClassesAsEnvironment: expectedResultEnvironment