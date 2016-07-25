event handling
uncheck
	
	super uncheck.
	
	self isSelected ifFalse: [ self hideActionbar ]