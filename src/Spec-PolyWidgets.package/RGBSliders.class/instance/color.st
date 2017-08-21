accessing
color
	^ (Color
				r: redSlider value
				g: greenSlider value
				b: blueSlider value
				range: 255)