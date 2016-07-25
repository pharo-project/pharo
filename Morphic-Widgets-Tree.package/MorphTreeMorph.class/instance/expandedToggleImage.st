expanding-collapsing
expandedToggleImage
	^ expandedToggleImage ifNil: [expandedToggleImage := ImageMorph new form: self expandedForm].
