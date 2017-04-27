*Reflectivity
installLink: aMetaLink
	self reflectiveMethod ifNil: [ self createTwin ].
	self reflectiveMethod increaseLinkCount.
	(aMetaLink optionCompileOnLinkInstallation or: [ self isRealPrimitive ])
		ifTrue: [ self reflectiveMethod compileAndInstallCompiledMethod ]
		ifFalse: [ self invalidate ].
	SystemAnnouncer uniqueInstance announce: (MetalinkChanged new link: aMetaLink)	
	