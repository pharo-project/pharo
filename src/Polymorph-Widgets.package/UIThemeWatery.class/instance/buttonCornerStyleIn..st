border-styles-buttons
buttonCornerStyleIn: aThemedMorph
	"Allow for themes to override default behaviour."
	
	^aThemedMorph
		ifNil: [#rounded]
		ifNotNil: [:tm | 
			tm preferredButtonCornerStyle
				ifNil: [#rounded]
				ifNotNil: [:bcs | bcs]]