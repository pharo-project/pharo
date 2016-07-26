styles
itemLabelStyleFor: aBrick

	aBrick label
		highlightingLogic: [ :aCanvas :aRectangle | |bounds|
			"because font calculates text width wrong"
			bounds := aRectangle translateBy: (-3@0).
			aCanvas line: bounds bottomLeft to: bounds bottomRight width: 1 color: Color lightGray ].
	
	aBrick
		hSpaceFill;
		vSpaceFill;
		marginRight: self outerBorderWidth.