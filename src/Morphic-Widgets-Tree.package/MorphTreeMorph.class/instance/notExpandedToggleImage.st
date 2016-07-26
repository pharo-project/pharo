expanding-collapsing
notExpandedToggleImage
	^ notExpandedToggleImage ifNil: [notExpandedToggleImage := ImageMorph new form: self notExpandedForm].
