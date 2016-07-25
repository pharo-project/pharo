instance creation
h: hue s: saturation l: lightness
	"Create a color with the given hue, saturation, and lightness. Hue is given as the angle in degrees of the color on the color circle where red is zero degrees. Saturation and lightness are numbers in [0.0..1.0] where larger values are more saturated or lighter colors. The difference with brightness in the HSV coordinates is that colors go from black at lightness 0, through vivid hues at lightness 0.5, to white at lightness 1. For example, (Color h: 0 s: 1 l: 0.5) is pure red."

	^ self
		h: hue
		s: saturation
		l: lightness
		alpha: 1.0