helper
aRandomSelectionOfCodePointsDo: aBlock
	0 to: 255 do: [ :cp | aBlock value: cp].
	500 timesRepeat: [ aBlock value: (unicodeGenerator randomCodePointAtOrAbove: 256) ].
