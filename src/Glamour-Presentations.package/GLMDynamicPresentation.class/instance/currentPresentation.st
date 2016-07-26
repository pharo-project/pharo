rendering
currentPresentation 
	| composite |
	composite := GLMCompositePresentation new.
	self presentationTransformation glamourValue: (
			composite asGlamorousMultiValue, 
			self displayValue asGlamorousMultiValue).
	composite pane: self pane.
	composite initializePresentation.
	cachedPresentation := composite.
	^ composite
"	^ self displayValue pane: self pane"