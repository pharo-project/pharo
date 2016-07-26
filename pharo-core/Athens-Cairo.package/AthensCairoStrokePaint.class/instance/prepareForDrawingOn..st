private
prepareForDrawingOn: aCanvas
	
	fillPaint 
		loadOnCairoCanvas: aCanvas.
	
	aCanvas
		primSetLineWidth: width;
		primSetLineJoin: joinStyle;
		primSetLineCap: capStyle.
	
	dashLenghts 
		ifNil: [ aCanvas resetDash ] 
		ifNotNil: [ self setDashesOn: aCanvas ].