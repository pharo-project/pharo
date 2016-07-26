conversions
halfTonePattern2
	"Return a halftone-pattern to approximate luminance levels on 2-bit deep Forms."
	| lum |
	lum := self luminance.
	lum < 0.125 ifTrue: [ ^ Bitmap with: 1431655765 ].	"black"
	lum < 0.25 ifTrue: 
		[ ^ Bitmap 
			with: 1431655765
			with: 3722304989 ].	"1/8 gray"
	lum < 0.375 ifTrue: 
		[ ^ Bitmap 
			with: 3722304989
			with: 2004318071 ].	"2/8 gray"
	lum < 0.5 ifTrue: 
		[ ^ Bitmap 
			with: 4294967295
			with: 2004318071 ].	"3/8 gray"
	lum < 0.625 ifTrue: [ ^ Bitmap with: 4294967295 ].	"4/8 gray"
	lum < 0.75 ifTrue: 
		[ ^ Bitmap 
			with: 4294967295
			with: 3149642683 ].	"5/8 gray"
	lum < 0.875 ifTrue: 
		[ ^ Bitmap 
			with: 4008636142
			with: 3149642683 ].	"6/8 gray"
	lum < 1.0 ifTrue: 
		[ ^ Bitmap 
			with: 2863311530
			with: 3149642683 ].	"7/8 gray"
	^ Bitmap with: 2863311530	"opaque white"

	"handy expression for computing patterns for 2x2 tiles;
 set p to a string of 4 letters (e.g., 'wggw' for a gray-and-
 white checkerboard) and print the result of evaluating:
| p d w1 w2 |
p := 'wggw'.
d := Dictionary new.
d at: $b put: '01'.
d at: $w put: '10'.
d at: $g put: '11'.
w1 := (d at: (p at: 1)), (d at: (p at: 2)).
w1 := '2r', w1, w1, w1, w1, w1, w1, w1, w1, ' hex'.
w2 := (d at: (p at: 3)), (d at: (p at: 4)).
w2 := '2r', w2, w2, w2, w2, w2, w2, w2, w2, ' hex'.
Array with: (Compiler evaluate: w1) with: (Compiler evaluate: w2) 
"