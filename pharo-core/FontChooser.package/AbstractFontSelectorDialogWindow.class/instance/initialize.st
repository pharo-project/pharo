initialization
initialize
	"Initialize the receiver."

	self
		isBold: false;
		isItalic: false;
		isUnderlined: false;
		isStruckOut: false;
		previewText: self defaultPreviewText;
		fontFamilyIndex: 0;
		fontSizeIndex: 0.
	super initialize