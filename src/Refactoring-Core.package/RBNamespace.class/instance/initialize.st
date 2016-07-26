initialization
initialize
	changes := RBCompositeRefactoryChange new.
	environment := RBBrowserEnvironment new.
	newClasses := IdentityDictionary new.
	changedClasses := IdentityDictionary new.
	removedClasses := Set new.
	implementorsCache := IdentityDictionary new.
	sendersCache := IdentityDictionary new