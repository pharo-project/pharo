accessing
blackGradientMorph
	"Answer the black gradient morph."

	^Morph new
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		fillStyle: self blackGradient