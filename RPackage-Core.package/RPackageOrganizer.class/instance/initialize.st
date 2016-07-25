initialization
initialize
	super initialize.
	
	packages := IdentityDictionary new.
	classPackageMapping := IdentityDictionary new.
	classExtendingPackagesMapping := IdentityDictionary new.
	debuggingName := ''.
	
	self defineUnpackagedClassesPackage