accessing
desktopImageLayout
	"Answer the layout symbol for any desktop image.
	See AlphaImageMorph>>layoutSymbols."

	^desktopImageLayout ifNil: [desktopImageLayout := #scaled]