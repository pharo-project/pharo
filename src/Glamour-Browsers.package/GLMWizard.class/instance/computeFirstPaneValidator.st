building internal
computeFirstPaneValidator

	|tmpValidator|
	tmpValidator := self computePaneValidator.
	self setFirstPaneButtonsTo: tmpValidator.
	^ tmpValidator  
