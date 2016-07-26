A TextLine embodies the layout of a line of composed text.
	left right top bottom		The full line rectangle
	firstIndex lastIndex		Starting and stopping indices in the full text
	internalSpaces		Number of spaces to share paddingWidth
	paddingWidth		Number of pixels of extra space in full line
	baseline				Distance of baseline below the top of the line
	leftMargin			Left margin due to paragraph indentation
TextLine's rather verbose message protocol is required for compatibility with the old CharacterScanners.