transformations
adjustSaturation: saturation brightness: brightness
	"Adjust the relative saturation and brightness of this color. (lowest value is 0.005 so that hue information is not lost)"

	^ self class
		h: self hue
		s: (self saturation + saturation min: 1.0 max: 0.005)
		v: (self brightness + brightness min: 1.0 max: 0.005)
		alpha: self alpha