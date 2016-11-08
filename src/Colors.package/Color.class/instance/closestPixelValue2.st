conversions
closestPixelValue2
	"Return the nearest approximation to this color for a 2-bit deep Form."
	"fast special cases"
	| lum |
	rgb = 0 ifTrue: [ ^ 1 ].	"black"
	rgb = 1073741823 ifTrue: [ ^ 2 ].	"opaque white"
	lum := self luminance.
	lum < 0.2 ifTrue: [ ^ 1 ].	"black"
	lum > 0.6 ifTrue: [ ^ 2 ].	"opaque white"
	^ 3	"50% gray"