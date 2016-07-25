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
	allClasses := SystemNavigation new allClasses.
	self assert: (factory createdClasses noneSatisfy: [:class| allClasses includes: class]).
	self assert: (SystemOrganization categoriesMatching: factory packageName, '*') isEmpty. 
	self deny: (ChangeSet current changedClassNames includesAnyOf: createdClassNames)
