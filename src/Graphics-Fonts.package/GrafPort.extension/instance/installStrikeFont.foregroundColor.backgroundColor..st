*Graphics-Fonts
installStrikeFont: aStrikeFont foregroundColor: foregroundColor backgroundColor: backgroundColor
	super installStrikeFont: aStrikeFont foregroundColor: foregroundColor backgroundColor: backgroundColor.
	aStrikeFont glyphs depth = 1 ifTrue: [
		alpha := foregroundColor privateAlpha.
		"dynamically switch between blend modes to support translucent text"
		"To handle the transition from TTCFont to StrikeFont, rule 34 must be taken into account."
		alpha = 255 ifTrue:[
			combinationRule = 30 ifTrue: [combinationRule := Form over].
			combinationRule = 31 ifTrue: [combinationRule := Form paint].
			combinationRule = 34 ifTrue: [combinationRule := Form paint].
			combinationRule = 41 ifTrue: [combinationRule := Form paint]. "41 is  SPRmode"
		] ifFalse:[
			combinationRule = Form over ifTrue: [combinationRule := 30].
			combinationRule = Form paint ifTrue: [combinationRule := 31].
			combinationRule = 34 ifTrue: [combinationRule := 31].
			combinationRule = 41 ifTrue: [combinationRule := 31]. "41 is SPR mode"
		]
	].
	lastFont := aStrikeFont.
	lastFontForegroundColor := foregroundColor.
	lastFontBackgroundColor := backgroundColor.
