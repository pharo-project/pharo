morph creation
newStringIn: aThemedMorph label: aStringOrText font: aFont style: aStyle
	"Answer a new string/text morph."

	^(EmbossedStringMorph contents: aStringOrText font: aFont)
		style: aStyle