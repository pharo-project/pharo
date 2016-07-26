character shapes
ensureCleanBold
	"This ensures that all character glyphs have at least one pixel of white space on the right
	so as not to cause artifacts in neighboring characters in bold or italic."
	| wider glyph |
	emphasis = 0 ifFalse: [ ^ self ].
	minAscii 
		to: maxAscii
		do: 
			[ :i | 
			glyph := self characterFormAt: (Character value: i).
			(glyph copy: (glyph boundingBox topRight - (1 @ 0) corner: glyph boundingBox bottomRight)) isAllWhite ifFalse: 
				[ wider := Form 
					extent: (glyph width + 1) @ glyph height
					depth: glyph depth.
				glyph depth > 1 ifTrue: [ wider fillWhite ].
				glyph displayOn: wider.
				self 
					characterFormAt: (Character value: i)
					put: wider ] ]
	"
StrikeFont allInstancesDo: [:f | f ensureCleanBold].
(StrikeFont familyName: 'NewYork' size: 21) ensureCleanBold.
StrikeFont shutDown.  'Flush synthetic fonts'.
"