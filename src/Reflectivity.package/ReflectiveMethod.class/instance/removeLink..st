invalidate
removeLink: aMetaLink
	(aMetaLink optionCompileOnLinkInstallation or: [ compiledMethod isRealPrimitive ])
		ifTrue: [ self compileAndInstallCompiledMethod ]
		ifFalse: [ compiledMethod invalidate ].
	self decreaseLinkCount.
	self linkCount = 0 ifTrue: [ self destroyTwin ].
	SystemAnnouncer uniqueInstance announce: (MetalinkChanged new link: aMetaLink).
		