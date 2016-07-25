initialize-release
scanner: aScanner 
	scanner := aScanner.
	pragmas := nil.
	aScanner errorBlock: self errorBlock.
	self initialize.
	self step.