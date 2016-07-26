as yet unclassified
xTableEncoding: anInteger
	"Answer an Array of the left x-coordinate of characters in glyphs."

	^(fontArray at: anInteger + 1) xTable.
