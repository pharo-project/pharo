button styles
styleButton: button morph: m pressed: aBoolean style: aSymbol
	(self isGrayRoundedButton: aSymbol)
		ifTrue:[ self grayRoundedButtonStyle: button morph: m pressed: aBoolean ].
	
	(self isWhiteRectangledButton: aSymbol)
		ifTrue:[ self whiteRectangledButtonStyle: button morph: m pressed: aBoolean ]