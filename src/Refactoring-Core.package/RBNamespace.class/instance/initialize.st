initialization
initialize
	changes := RBRefactoryChangeManager changeFactory compositeRefactoryChange.
	environment := RBBrowserEnvironment new.
	newClasses := IdentityDictionary new.
	changedClasses := IdentityDictionary new.
	removedClasses := Set new.
	implementorsCache := IdentityDictionary new.
	sendersCache := IdentityDictionary new