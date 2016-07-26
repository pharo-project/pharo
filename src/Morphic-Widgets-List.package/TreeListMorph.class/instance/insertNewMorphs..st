private
insertNewMorphs: morphList
	"Update the font on each morph."
	
	morphList do: [:m |
		m
			font: self font;
			extent: m minWidth @ m minHeight].
	^super insertNewMorphs: morphList