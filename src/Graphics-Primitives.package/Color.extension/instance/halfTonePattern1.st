*Graphics-Primitives
halfTonePattern1
	"Return a halftone-pattern to approximate luminance levels on 1-bit deep Forms."
	| lum |
	lum := self luminance.
	lum < 0.1 ifTrue: [ ^ Bitmap with: 4294967295 ].	"black"
	lum < 0.4 ifTrue: 
		[ ^ Bitmap 
			with: 3149642683
			with: 4008636142 ].	"dark gray"
	lum < 0.6 ifTrue: 
		[ ^ Bitmap 
			with: 1431655765
			with: 2863311530 ].	"medium gray"
	lum < 0.9 ifTrue: 
		[ ^ Bitmap 
			with: 1145324612
			with: 286331153 ].	"light gray"
	^ Bitmap with: 0	"1-bit white"