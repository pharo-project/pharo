initialization
initialize

	super initialize.
	self signature: self class defaultSignature.
	self version: self class currentVersion.
	header := FLHeader new.