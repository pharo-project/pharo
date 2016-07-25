private-primitives
primGetKerningLeft: leftGlyphIndex right: rightGlyphIndex
	"self primGetKerningLeft: $V asInteger  right: $a asInteger "
	<primitive: 'primitiveGetKerningLeftRight' module: 'FT2Plugin'>
	^self primitiveFailed.