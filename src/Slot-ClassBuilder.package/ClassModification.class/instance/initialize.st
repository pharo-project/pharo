initialization
initialize
	super initialize.
	changes := IdentityDictionary new.
	copies := IdentityDictionary new.
	removals := {}.
	additions := {}.