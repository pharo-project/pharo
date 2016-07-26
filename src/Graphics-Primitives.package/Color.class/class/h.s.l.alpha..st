instance creation
h: hue s: saturation l: lightness alpha: alpha
	^ self basicNew
		initializeHue: hue
			saturation: saturation
			lightness: lightness
			alpha: alpha;
		yourself