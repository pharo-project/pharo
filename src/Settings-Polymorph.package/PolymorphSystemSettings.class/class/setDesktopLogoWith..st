desktop
setDesktopLogoWith: aMorph
	self desktopLogo ifNotNil: [self desktopLogo delete].
	self desktopLogo: aMorph.
	self desktopLogo ifNotNil: [ :logo |
		logo beSticky; lock.
		World backgroundMorph
		ifNil: [ World addMorphBack: logo ] 
		ifNotNil: [ 	World addMorph: logo inFrontOf: World backgroundMorph ]
	]