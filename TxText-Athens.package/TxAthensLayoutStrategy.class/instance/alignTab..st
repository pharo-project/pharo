as yet unclassified
alignTab: font
	| tabWidth lineWidth |
	tabWidth := self layout tabWidthUsingFont: font.
	lineWidth := currentLine width.
	
	^ lineWidth // tabWidth + 1 * tabWidth - lineWidth @ 0
	