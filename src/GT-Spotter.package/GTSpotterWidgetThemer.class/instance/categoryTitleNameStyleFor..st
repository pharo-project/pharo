styles
categoryTitleNameStyleFor: aBrick

	aBrick
		vSpaceFill;
		textPosition: #leftCenter;
		width: [ :name | (aBrick owner innerBounds width) min: ((name label textSize x) + name label hMargin) ].
	
	aBrick label
		familyName: self titleFamilyName.
	