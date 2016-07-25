cleaning
cleanUp
	| createdClassNames createdTraitNames |
	createdClassNames := self createdClassNames.
	createdTraitNames := self createdTraitNames.
	self deleteClasses.
	self deleteTraits.
	self deletePackage.
	self cleanUpChangeSetForClassNames: createdClassNames.
	self cleanUpChangeSetForClassNames: createdTraitNames