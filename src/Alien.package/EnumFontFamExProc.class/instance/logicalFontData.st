accessing
logicalFontData
	"Convenience for ENUMLOGFONTEX *lpelfe,    // logical-font data"
	^ENUMLOGFONTEXA atAddress: (self unsignedLongAt: 1)