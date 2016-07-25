border-styles
tabGroupCornerStyleIn: aThemedMorph
	"Allow for themes to override default behaviour."
	
	^aThemedMorph
		ifNil: [#square]
		ifNotNil: [:tm | tm preferredCornerStyle]