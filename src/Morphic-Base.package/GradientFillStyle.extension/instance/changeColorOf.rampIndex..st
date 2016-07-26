*Morphic-Base-Balloon
changeColorOf: aMorph rampIndex: rampIndex
	| originalColor |
	originalColor := (colorRamp at: rampIndex) value.
	Smalltalk ui theme 
		chooseColorIn: World title: 'Color' color: originalColor for: [:color |
		self changeColorAt: rampIndex  to:  color.
		aMorph changed] 