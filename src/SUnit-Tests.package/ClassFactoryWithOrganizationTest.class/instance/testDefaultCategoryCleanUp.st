testing
testDefaultCategoryCleanUp
	| createdClassNames allClasses |
	3 timesRepeat: [
		factory newClass].
	createdClassNames := factory createdClassNames.
	factory cleanUp.	
	self assert: (factory createdClasses allSatisfy: [:class| class isObsolete]). 
	allClasses := self testedEnvironment allClasses.
	self assert: (factory createdClasses noneSatisfy: [:class| allClasses includes: class]).
	self deny: (self testedOrganization categories includes: factory defaultCategory). 
	self class environment at: #ChangeSet ifPresent: [:changeSet |
		self deny: (changeSet current changedClassNames includesAnyOf: createdClassNames) ].
