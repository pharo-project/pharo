factory
buildWidget
	| alphaImage |
	alphaImage := AlphaImageMorph new.
	alphaImage model: self.
	alphaImage
		getImageSelector: #getImage;
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		layout: #center;
		dragEnabled: self dragEnabled;
		dropEnabled: self dropEnabled;
		setBalloonText: self help;
		update: #getImage.
	^ alphaImage