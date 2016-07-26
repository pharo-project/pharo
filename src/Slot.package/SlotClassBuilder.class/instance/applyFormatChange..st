private
applyFormatChange: classModification
	result := self applyAndUpdateFormat: classModification.
	classModification allPropagationsDo: [ :propagation | 
		self applyAndUpdateFormat: propagation ]