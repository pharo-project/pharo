private
classAndMetaClassRuleFor: aTestSelector plusClasses: aCollectionOfTestedClasses
	| result expectedResultClasses expectedResultEnvironment |
	result := self
		runRuleFor: aTestSelector
		onEnvironment: self classEnvironmentWithAllObjectClasses.
	expectedResultClasses :=
		(aCollectionOfTestedClasses copyWith: (self objectClassFor: aTestSelector)).
	expectedResultEnvironment :=
		self classEnvironmentWithAll: expectedResultClasses.
	expectedResultClasses do: [ :each |
		expectedResultEnvironment addClass: each theMetaClass ].
	self
		assertResultOfRule: result
		hasSameClassesAsEnvironment: expectedResultEnvironment