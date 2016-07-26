initialize-release
initialize
	super initialize.
	prettyPrint := false.
	newLine := String cr.
	level := 0.
	referencePolicy := #normal.
	jsonMode := keepNewLines := asciiOnly := false.
	objects := IdentityDictionary new