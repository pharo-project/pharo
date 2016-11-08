conversions
initializeHue: hue saturation: saturation brightness: brightness alpha: anAlpha
	"Initialize this color to the given hue, saturation, and brightness. See the comment in the instance creation method for details."

	| s v hf i f p q t |
	rgb == nil
		ifFalse: [ self attemptToMutateError ].
	s := saturation asFloat min: 1.0 max: 0.0.
	v := brightness asFloat min: 1.0 max: 0.0.
	
	"zero saturation yields gray with the given brightness"
	s = 0.0 ifTrue: [
		^ self
			initializeRed: v
			green: v
			blue: v
			alpha: anAlpha ].
	
	hf := hue asFloat \\ 360.0 / 60.0.
	i := hf asInteger.	"integer part of hue"
	f := hf fractionPart.	"fractional part of hue"
		
	p := (1.0 - s) * v.
	q := (1.0 - (s * f)) * v.
	t := (1.0 - (s * (1.0 - f))) * v.
	
	0 = i
		ifTrue: [ 
			^ self
				initializeRed: v
				green: t
				blue: p
				alpha: anAlpha ].
	1 = i
		ifTrue: [ 
			^ self
				initializeRed: q
				green: v
				blue: p
				alpha: anAlpha ].
	2 = i
		ifTrue: [ 
			^ self
				initializeRed: p
				green: v
				blue: t
				alpha: anAlpha ].
	3 = i
		ifTrue: [ 
			^ self
				initializeRed: p
				green: q
				blue: v
				alpha: anAlpha ].
	4 = i
		ifTrue: [ 
			^ self
				initializeRed: t
				green: p
				blue: v
				alpha: anAlpha ].
	5 = i
		ifTrue: [ 
			^ self
				initializeRed: v
				green: p
				blue: q
				alpha: anAlpha ].
	self error: 'implementation error'