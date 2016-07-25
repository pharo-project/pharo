class initialization
initialize 
	self canBeInstalled 
		ifTrue: [ EmbeddedFreeTypeFontInstaller registerFont: self ]