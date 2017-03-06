updating
themeChanged

	super themeChanged.
	
	progressMorph color: self theme backgroundColor.
	progressMorph submorphs
		select: [ :each | each class == StringMorph ]
		thenDo: [ :each | each color: self theme disabledTextColor ]