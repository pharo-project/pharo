instance creation
hueShift: hueShift rotations: rotations saturation: saturation
	^ self new
		hueShift: hueShift;
		rotations: rotations;
		saturation: saturation;
		yourself