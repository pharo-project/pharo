private
computeFramedColors
	| base light dark w hw colorArray param |
	base := self color asColor.
	light := Color white.
	dark := Color black.
	w := self width isPoint ifTrue:[self width x max: self width y] ifFalse:[self width].
	w := w asInteger.
	w = 1 ifTrue:[^{base mixed: 0.5 with: light. base mixed: 0.5 with: dark}].
	colorArray := Array new: w.
	hw := w // 2.
	"brighten"
	0 to: hw-1 do:[:i|
		param := 0.5 + (i asFloat / hw * 0.5).
		colorArray at: i+1 put: (base mixed: param with: light). "brighten"
		colorArray at: w-i put: (base mixed: param with: dark). "darken"
	].
	w odd ifTrue:[colorArray at: hw+1 put: base].
	^colorArray, colorArray