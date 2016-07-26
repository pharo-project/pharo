building internal
computeLastPaneValidator

	|tmpValidator|
	tmpValidator := self computePaneValidator.
	self setLastPaneButtonsTo: tmpValidator.
	^ tmpValidator  
