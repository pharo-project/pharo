testing
testPackageCleanUp
	| createdClassNames allClasses |
	3 timesRepeat: [
		factory newClassInCategory: #One].
	2 timesRepeat: [
		factory newClassInCategory: #Two].
	createdClassNames := factory createdClassNames.
	factory cleanUp.	
	self assert: (factory createdClasses allSatisfy: [:class| class isObsolete]). 
	allClasses := self testedEnvironment allClasses.
	self assert: (factory createdClasses noneSatisfy: [:class| allClasses includes: class]).
	self assert: (self testedOrganization categoriesMatching: factory packageName, '*') isEmpty. 
	self deny: (ChangeSet current changedClassNames includesAnyOf: createdClassNames).
