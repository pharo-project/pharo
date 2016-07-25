font registration
registerFonts: freeTypeFontManager
	{ SourceSansProRegular. SourceCodeProRegular } do: [ :each |
		| font |
		font := each new.
		freeTypeFontManager 
			addFromFileContents: font fontContents 
			baseName: font originalFileName ]