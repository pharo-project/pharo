drawing
drawOn: aCanvas

	(Display depth < 3 or: [ aCanvas isShadowDrawing ]) ifTrue: [ ^ self ].		
	
	self computeSelectionShapesFor: aCanvas clipRect.
	self cleanUpText.
	self drawAllSelectionShapesOn: aCanvas