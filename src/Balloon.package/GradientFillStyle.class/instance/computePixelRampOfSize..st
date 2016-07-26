private
computePixelRampOfSize: length
	"Compute the pixel ramp in the receiver"
	| bits lastValue ramp lastColor lastIndex lastWord |
	ramp := colorRamp asSortedCollection:[:a1 :a2| a1 key < a2 key].
	bits := Bitmap new: length.
	lastColor := ramp first value.
	lastWord := lastColor pixelWordForDepth: 32.
	lastIndex := 0.
	ramp do:[:assoc| | theta nextIndex nextColor nextWord distance step |
		nextIndex := (assoc key * length) rounded.
		nextColor := assoc value.
		nextWord := nextColor pixelWordForDepth: 32.
		distance := (nextIndex - lastIndex).
		distance = 0 ifTrue:[distance := 1].
		step := 1.0 / distance asFloat.
		theta := 0.0.
		lastIndex+1 to: nextIndex do:[:i|
			theta := theta + step.
			"The following is an open-coded version of:
				color := nextColor alphaMixed: theta with: lastColor.
				bits at: i put: (color scaledPixelValue32).
			"
			bits at: i put: (self scaledAlphaMix: theta of: lastWord with: nextWord).
		].
		lastIndex := nextIndex.
		lastColor := nextColor.
		lastWord := nextWord.
	].
	lastValue := lastColor scaledPixelValue32.
	lastIndex+1 to: length do:[:i| bits at: i put: lastValue].
	^bits