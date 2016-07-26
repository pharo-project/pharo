geometry
optimalExtent
	"Answer the extent of the list morph."

	^self listMorph extent + (self borderWidth * 2) + self scrollBarThickness