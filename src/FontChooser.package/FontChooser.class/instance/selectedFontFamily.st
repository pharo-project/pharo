selected
selectedFontFamily

	^ self fontList at: self selectedFontIndex ifAbsent: [nil].
	
	