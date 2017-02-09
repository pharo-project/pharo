initialize-release
initialize
	super initialize.
	objects := IdentityDictionary new.
	classes := IdentityDictionary new.
	acceptUnknownClasses := convertNewLines := false.
	newLine := String cr.
	unresolvedReferences := 0