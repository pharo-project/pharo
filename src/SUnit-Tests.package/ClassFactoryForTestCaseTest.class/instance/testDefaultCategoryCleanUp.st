testing
testDefaultCategoryCleanUp
	| createdClassNames allClasses allTraits |
	
	3 timesRepeat: [
		factory newClass.
		factory newTrait].
	
	createdClassNames := factory createdClassNames.
	
	factory cleanUp.	
	
	self assert: (factory createdClasses allSatisfy: [:class| class isObsolete]). 
	self assert: (factory createdTraits allSatisfy: [:trait| trait isObsolete]). 
	
	allClasses := SystemNavigation new allClasses.
	allTraits := Smalltalk globals allTraits.
	
	self assert: (factory createdClasses noneSatisfy: [:class| allClasses includes: class]).
	self assert: (factory createdTraits noneSatisfy: [:trait| allTraits includes: trait]).
	self deny: (SystemOrganization categories includes: factory defaultCategory). 
	self class environment at: #ChangeSet ifPresent: [:changeSet |
		self deny: (changeSet current changedClassNames includesAnyOf: createdClassNames) ].
