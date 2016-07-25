accessing
hue
	"Return the hue of this color, an angle in the range [0.0..360.0]."
	| r g b max min span h |
	r := self privateRed.
	g := self privateGreen.
	b := self privateBlue.
	max := (r max: g) max: b.
	min := (r min: g) min: b.
	span := (max - min) asFloat.
	span = 0.0 ifTrue: [ ^ 0.0 ].
	r = max 
		ifTrue: [ h := (g - b) asFloat / span * 60.0 ]
		ifFalse: 
			[ g = max 
				ifTrue: [ h := 120.0 + ((b - r) asFloat / span * 60.0) ]
				ifFalse: [ h := 240.0 + ((r - g) asFloat / span * 60.0) ] ].
	h < 0.0 ifTrue: [ h := 360.0 + h ].
	^ h