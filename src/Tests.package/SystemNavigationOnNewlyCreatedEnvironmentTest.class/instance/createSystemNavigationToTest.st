setUp-tearDown
createSystemNavigationToTest
	
	| environment nav |
	environment := SystemDictionary withOrganizer: SystemOrganizer new.
	nav := SystemNavigation new.
	nav environment: environment.
	^nav