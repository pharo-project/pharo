styles
searchFieldStyleFor: aBrick
	
	aBrick
		beCentered;
		hSpaceFill;
		vSpaceFill;
		withoutMargins;
		withoutAdornment;
		withoutBorder;
		beTransparent;
		withGhostText: self spotterGhostText;
		autoAccept: true;
		marginLeft: self searchFieldMarginLeft;
		rubricTextFamilyName: self searchFieldFamilyName;
		rubricTextFontSize: self searchFieldFontSize.
	
	aBrick rubric ghostTextRuler
		fontName: self searchFieldFamilyName pointSize: self searchFieldFontSize.