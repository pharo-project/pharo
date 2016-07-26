figures
figure4: aCanvas
	| path stroke |
	
	path := self roundedRectanglePathOn: aCanvas.
	
	aCanvas setShape: path.
	
	aCanvas setPaint: self carColor.
	aCanvas draw.
	
	stroke := aCanvas setStrokePaint: Color black.
	stroke width: 3.
	
	aCanvas draw. 