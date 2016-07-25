drawing
drawOn: canvas
	| px py |
	canvas fillRectangle: bounds color: self backgroundColor.
	self renderScene.

	surface displayOnMorphicCanvas: canvas at: bounds origin.

"	 translucentImage: surface asForm at: bounds origin."
	
	self showDebugInfo
		ifTrue: [ 
			px := transform x printShowingDecimalPlaces: 3.
			py := transform y printShowingDecimalPlaces: 3.
			canvas
				drawString: 'zoom: ' , (transform sx printShowingDecimalPlaces: 3) , ' pan: ' , px , ' @ ' , py
				at: bounds origin
				font: nil
				color: Color white ]