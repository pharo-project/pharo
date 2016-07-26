system startup
updateFreeType
	self current 
		clearBitBltSubPixelAvailable;
		clearForceNonSubPixelCount.
	"Embeded fonts update always"
	FreeTypeFontProvider current updateEmbeddedFreeTypeFonts. 
	self updateFontsAtImageStartup 
		ifTrue: [ FreeTypeFontProvider current updateFromSystem ]