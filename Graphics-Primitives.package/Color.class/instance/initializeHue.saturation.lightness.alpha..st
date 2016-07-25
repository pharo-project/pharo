conversions
initializeHue: hue saturation: saturation lightness: lightness alpha: anAlpha
	"Initialize this color to the given hue, saturation, and lightness. See the comment in the instance creation method for details."

	| s l hf i f lo hi up dn |
	rgb == nil
		ifFalse: [ self attemptToMutateError ].
	s := saturation asFloat min: 1.0 max: 0.0.
	l := lightness asFloat min: 1.0 max: 0.0.

	"zero saturation yields gray with the given lightness"
	s = 0.0
		ifTrue: [ ^ self initializeRed: l green: l blue: l alpha: anAlpha ].

	hf := hue asFloat \\ 360.0 / 60.0.
	i := hf asInteger.	"integer part of hue"
	f := hf fractionPart.	"fractional part of hue"

	"Color components follow a step function between levels hi & lo with sloped transitions up & dn. Each component is phased according to hue."
	hi := l < 0.5 ifTrue: [(1.0 + s) * l] ifFalse: [l + s - (l * s)].
	lo := 2.0 * l - hi.
	up := (hi - lo) * f + lo.
	dn := (lo - hi) * f + hi.

	0 = i ifTrue: [ ^ self initializeRed: hi green: up blue: lo alpha: anAlpha ].
	1 = i ifTrue: [ ^ self initializeRed: dn green: hi blue: lo alpha: anAlpha ].
	2 = i ifTrue: [ ^ self initializeRed: lo green: hi blue: up alpha: anAlpha ].
	3 = i ifTrue: [ ^ self initializeRed: lo green: dn blue: hi alpha: anAlpha ].
	4 = i ifTrue: [ ^ self initializeRed: up green: lo blue: hi alpha: anAlpha ].
	5 = i ifTrue: [ ^ self initializeRed: hi green: lo blue: dn alpha: anAlpha ].
	self error: 'implementation error'