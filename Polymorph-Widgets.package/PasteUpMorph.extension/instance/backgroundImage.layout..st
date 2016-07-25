*Polymorph-Widgets
backgroundImage: aForm layout: aSymbol
	"Set a background image, replacing the current background morph, if any,
	with the given layout (see AlphaImageMorph layoutSymbols). "

	self backgroundMorph: (
		(self theme builder
			newAlphaImage: aForm help: nil)
			autoSize: false;
			layout: aSymbol;
			lock)