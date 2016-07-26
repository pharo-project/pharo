event handling
updateContents
	row ifNil: [ ^self ].
	wrappingPolicyMorph contents: self wrappingPolicyLabel.
	editingModeMorph contents: self editingModeLabel.
	lineNumbersDisplayMorph contents: self lineNumbersDisplayLabel.
	row bounds: self innerBounds