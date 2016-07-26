accessing
current
	current ifNil: [ 
		current := self new 
			provider: FreeTypeFontProvider current; 
			yourself.
		FreeTypeFontProvider current addFontInstaller: self current ].
	^ current.