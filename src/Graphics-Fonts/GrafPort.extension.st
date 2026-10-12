Extension { #name : 'GrafPort' }

{ #category : '*Graphics-Fonts' }
GrafPort >> installStrikeFont: aStrikeFont foregroundColor: foregroundColor backgroundColor: backgroundColor scale: scale [

	super
		installStrikeFont: aStrikeFont
		foregroundColor: foregroundColor
		backgroundColor: backgroundColor
		scale: scale.
	aStrikeFont glyphs depth = 1 ifTrue: [
			self alpha: foregroundColor privateAlpha.
			"dynamically switch between blend modes to support translucent text"
			"To handle the transition from TTCFont to StrikeFont, rule 34 must be taken into account."
			self alpha = 255
				ifTrue: [
						self combinationRule = 30 ifTrue: [ self combinationRule: Form over ].
						self combinationRule = 31 ifTrue: [ self combinationRule: Form paint ].
						self combinationRule = 34 ifTrue: [ self combinationRule: Form paint ].
						self combinationRule = 41 ifTrue: [ self combinationRule: Form paint ] "41 is  SPRmode" ]
				ifFalse: [
						self combinationRule = Form over ifTrue: [ self combinationRule: 30 ].
						self combinationRule = Form paint ifTrue: [ self combinationRule: 31 ].
						self combinationRule = 34 ifTrue: [ self combinationRule: 31 ].
						self combinationRule = 41 ifTrue: [ self combinationRule: 31 ] "41 is SPR mode" ] ].
	self lastFont: aStrikeFont.
	self lastFontForegroundColor: foregroundColor.
	self lastFontBackgroundColor: backgroundColor
]
