private
setHue: hue saturation: saturation brightness: brightness 
	"Initialize this color to the given hue, saturation, and brightness. See the comment in the instance creation method for details."
	| s v hf i f p q t |
	s := (saturation asFloat max: 0.0) min: 1.0.
	v := (brightness asFloat max: 0.0) min: 1.0.

	"zero saturation yields gray with the given brightness"
	s = 0.0 ifTrue: 
		[ ^ self 
			setRed: v
			green: v
			blue: v ].
	hf := hue asFloat.
	(hf < 0.0 or: [ hf >= 360.0 ]) ifTrue: [ hf := hf - ((hf quo: 360.0) asFloat * 360.0) ].
	hf := hf / 60.0.
	i := hf asInteger.	"integer part of hue"
	f := hf fractionPart.	"fractional part of hue"
	p := (1.0 - s) * v.
	q := (1.0 - (s * f)) * v.
	t := (1.0 - (s * (1.0 - f))) * v.
	0 = i ifTrue: 
		[ ^ self 
			setRed: v
			green: t
			blue: p ].
	1 = i ifTrue: 
		[ ^ self 
			setRed: q
			green: v
			blue: p ].
	2 = i ifTrue: 
		[ ^ self 
			setRed: p
			green: v
			blue: t ].
	3 = i ifTrue: 
		[ ^ self 
			setRed: p
			green: q
			blue: v ].
	4 = i ifTrue: 
		[ ^ self 
			setRed: t
			green: p
			blue: v ].
	5 = i ifTrue: 
		[ ^ self 
			setRed: v
			green: p
			blue: q ].
	self error: 'implementation error'