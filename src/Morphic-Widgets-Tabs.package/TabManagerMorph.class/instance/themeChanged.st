initialization
themeChanged
	super themeChanged.
	toolbar themeChanged.
	
	container
		color: Smalltalk ui theme lightBaseColor;
		changed.
	contentsWrapper
		color: Smalltalk ui theme lightBaseColor;
		changed