drawning
drawOn: aCanvas 
	
	super drawOn: aCanvas.
	aCanvas paintImage: (images at: currentImageIndex) at: self position