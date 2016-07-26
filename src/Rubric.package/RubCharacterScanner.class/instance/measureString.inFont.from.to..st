scanning
measureString: aString inFont: aFont from: startIndex to: stopIndex
	"WARNING: In order to use this method the receiver has to be set up using #initializeStringMeasurer"
	destX := destY := lastIndex := 0.
	baselineY := aFont ascent.
	font := aFont.  " added Dec 03, 2004 "
"	map := aFont characterToGlyphMap."
	self scanCharactersFrom: startIndex to: stopIndex in: aString rightX: 999999 stopConditions: stopConditions kern: 0.
	^destX