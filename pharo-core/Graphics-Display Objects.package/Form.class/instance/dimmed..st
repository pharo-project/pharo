converting
dimmed: factor
	"Answer a dimmed variant of this form. factor in a float between 0 and 1"
	"(Form fromUser dimmed: 0.6) asMorph openInWorld"
	^ self collectColors: [ :color |
			color alpha: (color alpha min: factor)]