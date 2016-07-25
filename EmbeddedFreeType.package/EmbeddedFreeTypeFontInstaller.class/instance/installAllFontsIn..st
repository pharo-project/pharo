accessing
installAllFontsIn: aProvider
	self provider: aProvider.
	EmbeddedFreeTypeFontFontDescription installAllFontsIn: self.	
	embeddedFileInfoCache valuesDo: [ :eachSet | eachSet do: [ :each | provider addFileInfo: each ] ]