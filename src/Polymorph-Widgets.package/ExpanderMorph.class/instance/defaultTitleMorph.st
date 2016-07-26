initialization
defaultTitleMorph
	"Answer a default title morph for the receiver."

	^ExpanderTitleMorph basicNew
		basicTheme: self theme;
		initialize;
		hResizing: #spaceFill;
		vResizing: #shrinkWrap;
		basicTheme: nil