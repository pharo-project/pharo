font accessing
setAllStandardFontsTo: aFont
	self systemFontAspects do: [:selector | StandardFonts perform: (selector, ':') asSymbol with: aFont].
	 
