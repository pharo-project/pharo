drawing
drawSelectionOn: aCanvas

	controller selection isEmpty ifTrue: [ ^ self ].
	aCanvas pathTransform restoreAfter: [ 
		aCanvas pathTransform translateBy: self innerBounds topLeft.  
		aCanvas clipBy: (0@0 extent: self layoutView extent)  during: [ 
			self layoutView renderHighlight: controller selection with: self theme selectionColor on: aCanvas.
			]
	]