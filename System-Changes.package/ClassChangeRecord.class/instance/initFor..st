initialization
initFor: className

	changeTypes := IdentitySet new.
	methodChanges := IdentityDictionary new.
	priorName := thisName := className.