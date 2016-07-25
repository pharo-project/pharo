border-styles-buttons
buttonCornerStyleIn: aThemedMorph
	"Allow for themes to override default behaviour."
	
	^aThemedMorph
		ifNil: [#square]
		ifNotNil: [:tm | 
			tm preferredButtonCornerStyle
				ifNil: [tm preferredCornerStyle]
				ifNotNil: [:bcs | bcs]]