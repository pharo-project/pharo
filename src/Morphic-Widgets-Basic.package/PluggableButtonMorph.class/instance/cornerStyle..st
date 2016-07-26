accessing
cornerStyle: aSymbol
	"Adjust the layout inset."

	super cornerStyle: aSymbol.
	self layoutInset: (self theme buttonLabelInsetFor: self)