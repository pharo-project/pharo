private
onImage
	"Return the form to be used for indicating an on marker"
	| form |
	form := Form extent: (self fontToUse ascent-2) asPoint depth: 16.
	(form getCanvas)
		frameAndFillRectangle: form boundingBox fillColor: (Color gray: 0.8) 
			borderWidth: 1 borderColor: Color black;
		fillRectangle: (form boundingBox insetBy: 2) fillStyle: Color black.
	^form