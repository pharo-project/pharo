private
applySuperclassChange: classModification
	result := self applyAndUpdateSuperclass: classModification.
	classModification allPropagationsDo: [ :propagation | 
		self applyAndUpdateSuperclass: propagation ]