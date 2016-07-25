initialization
initialize
	super initialize.
	loaded := Set new.
	loadedPackages := Set new.
	versionPattern := ''.
	packagePattern := ''.
	packageList := #()