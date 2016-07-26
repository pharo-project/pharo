rounding
cornerStyle: aSymbol
	"Update the layout inset too."

	super cornerStyle: aSymbol.
	self layoutInset: self layoutInsetToUse.
	self buttonMorph cornerStyle: self cornerStyle.
	self fillStyle: self fillStyleToUse