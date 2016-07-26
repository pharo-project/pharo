converting
darker: aFactor
	"Answer a darker variant of this form. aFactor is a float between 0 and 1 representing the strength of 
	the darkening."
	"(Form fromUser darker: 0.08) asMorph openInWorld"
	"(Form fromUser darker: 0.16) asMorph openInWorld"
	
	^ self collectColors: [ :color |
				color adjustBrightness: aFactor negated]