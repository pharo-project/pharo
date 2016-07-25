accessing
setSourceCodeFonts: pointSize
	StandardFonts
		setAllStandardFontsTo: (LogicalFont familyName: self defaultFontName pointSize: pointSize);
		haloFont: (LogicalFont familyName: self defaultFontName pointSize: pointSize - 1);
		balloonFont: (LogicalFont familyName: self defaultFontName pointSize: pointSize - 1);
		windowTitleFont: (LogicalFont familyName: self defaultFontName pointSize: pointSize + 1);
		listFont: (LogicalFont familyName: self defaultFontName pointSize: pointSize);
		menuFont: (LogicalFont familyName: self defaultFontName pointSize: pointSize);
		codeFont: (LogicalFont familyName: self fontName pointSize: pointSize)