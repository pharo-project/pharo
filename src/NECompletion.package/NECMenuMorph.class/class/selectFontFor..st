preferences-fonts
selectFontFor: aSymbol 
	| emphasized attributes |
	attributes := SHTextStylerST80 new 
		attributesFor: (self convertToSHSymbol: aSymbol).
	emphasized := attributes size > 1 
		ifTrue: [ attributes second emphasisCode ]
		ifFalse: [ 0 ].
	^ StandardFonts codeFont
		emphasized: emphasized