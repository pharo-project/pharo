instance creation
for: globalSymbols in: globalEnvironment

	^ self basicNew
		initializeWith: (self valuesFor: globalSymbols in: globalEnvironment);
		yourself.