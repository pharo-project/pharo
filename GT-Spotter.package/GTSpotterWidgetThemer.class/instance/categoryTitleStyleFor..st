styles
categoryTitleStyleFor: aBrick

	aBrick
		useHorizontalLinearLayout;
		height: 30;
		hSpaceFill;
		paddingLeft: self categoryNameMarginLeft
	
	"aBrick layout
		floating: #stack;
		maxWidth: [ :title |
			aBrick hPadding + 
			(title nameBrick label textSize x )+ title amountBrick label textSize x +
			title nameBrick hPadding + title nameBrick hMargin + 
			title nameBrick label hPadding + title nameBrick label hMargin + 
			title amountBrick hMargin ]"