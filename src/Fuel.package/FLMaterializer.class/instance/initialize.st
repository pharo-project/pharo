initialization
initialize

	super initialize.
	self setDefaultMaterialization.
	self signature: self class defaultSignature.
	self version: self class currentVersion.
	migrations := Dictionary new.
	globalEnvironment := self class environment.