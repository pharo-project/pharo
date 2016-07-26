menu
autoFitOnOff
	self setProperty: #autoFitContents toValue: self isAutoFit not.
	self isAutoFit ifTrue: [self fit]