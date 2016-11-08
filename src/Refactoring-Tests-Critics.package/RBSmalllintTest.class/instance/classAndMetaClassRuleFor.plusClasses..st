private
classAndMetaClassRuleFor: aTestSelector plusClasses: aCollectionOfTestedClasses
	| rule expectedResultClasses expectedResultEnvironment |
	rule := self
		runRuleFor: aTestSelector
		onEnvironment: self classEnvironmentWithAllObjectClasses.
	expectedResultClasses :=
		(aCollectionOfTestedClasses copyWith: (self objectClassFor: aTestSelector)).
	expectedResultEnvironment :=
		self classEnvironmentWithAll: expectedResultClasses.
	expectedResultClasses do: [ :each |
		expectedResultEnvironment addClass: each theMetaClass ].
	self
		assertResultOfRule: rule
		hasSameClassesAsEnvironment: expectedResultEnvironment