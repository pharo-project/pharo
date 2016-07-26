loading and updating
updateEmbeddedFreeTypeFonts
	"Ensure the provider is installed"	
	self addFontInstaller: EmbeddedFreeTypeFontInstaller current.
	"Now install embeded fonts"
	fontInstallers do: [ :each | each installAllFontsIn: self ].
	self buildFamilies