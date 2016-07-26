invalidate
removeLink: aMetaLink
	aMetaLink optionCompileOnLinkInstallation ifTrue: [  
		self compileAndInstallCompiledMethod].
	self decreaseLinkCount.
	self linkCount = 0 ifTrue: [ self destroyTwin ].
	SystemAnnouncer uniqueInstance announce: (MetalinkChanged new link: aMetaLink)
		