system startup
shutDown: quitting 
	quitting ifFalse: [ ^ self ]. 

	self current 
		clearBitBltSubPixelAvailable;
		clearForceNonSubPixelCount.
	"FreeTypeCache clearCacheOnShutdown
		ifTrue: [ FreeTypeCache clearCurrent ].	
	FT2Handle clearRegistry.
	FreeTypeFontProvider resetCurrent. 
	EmbeddedFreeTypeFontInstaller resetCurrent."