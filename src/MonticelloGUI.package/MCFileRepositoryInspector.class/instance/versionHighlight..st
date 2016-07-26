morphic ui
versionHighlight: aString

	^ Text
		string: aString
		attribute: (self versionHighlightEmphasis: aString)