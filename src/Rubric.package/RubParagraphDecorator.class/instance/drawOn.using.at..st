drawing
drawOn: aCanvas using: aDisplayScanner at: aPosition

	(self canDrawDecoratorsOn: aCanvas) ifFalse: [ ^self ].
	
	self drawOn: aCanvas.
	next drawOn: aCanvas using: aDisplayScanner at: aPosition