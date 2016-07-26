drawing
drawAllSelectionShapesOn: aCanvas

	findReplaceShapes ifNotNil: [ 
		findReplaceShapes do: [ :selblock | 
			self drawSelectionShape: selblock on: aCanvas
		] 
	].

	secondarySelectionShapes ifNotNil: [ 
		secondarySelectionShapes do: [ :selblock | 
				self drawSelectionShape: selblock on: aCanvas 
		] 
	]