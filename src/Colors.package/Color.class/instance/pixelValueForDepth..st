conversions
pixelValueForDepth: d 
		 
		"Return the pixel value for this color at the given depth. Translucency only works in RGB; this color will appear either opaque or transparent at all other depths."
	| basicPixelWord |
	( d < 32 and:[ self isTransparent ] ) ifTrue: [ ^ 0 ].
	basicPixelWord := self basicPixelValueForDepth: d.
	^ d < 32 
		ifTrue: [ basicPixelWord ]
		ifFalse: [ (basicPixelWord bitAnd: 16777215) bitOr: (alpha bitShift: 24) ]