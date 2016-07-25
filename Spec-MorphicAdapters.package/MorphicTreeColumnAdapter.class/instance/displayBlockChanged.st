private
displayBlockChanged

	self widgetDo: [ :w | 
		w rowMorphGetSelector: self displayBlock.
		w container updateList ]