private
addSubmorphsAfter: parentMorph fromCollection: aCollection allowSorting: sortBoolean
	"Update the font on each morph."
	
	|answer|
	answer := super addSubmorphsAfter: parentMorph fromCollection: aCollection allowSorting: sortBoolean.
	"I commented out the following lines, because it makes this morph incredible slow.
	This is used to browse diffs from monticello which can be really big.
	I couldn't find any place where this Morph is used and needs the following lines.
	nicolai."
	self flag:#issue12027.
	"self scroller submorphsDo: [:i |
		i
			font: self font;
			extent: i minWidth @ i minHeight]."
	^answer