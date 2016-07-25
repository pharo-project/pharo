adding
computeStaticDependencies: aPackage
	
	self addInheritanceDependencies: aPackage.
	self addExtensionDependencies: aPackage.
	self addReferenceDependencies: aPackage.
	self addTraitDependencies: aPackage.
	self addMessageSendDependencies: aPackage