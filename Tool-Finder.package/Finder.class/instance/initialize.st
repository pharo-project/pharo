initialization
initialize
	super initialize.
	searchingString := self defaultString.
	environment := self defaultEnvironment.
	packagesSelection := self defaultPackagesSelection.
	resultDictionary := Dictionary new.
	currentSearchMode := #Selectors.
	useRegEx := false.
	