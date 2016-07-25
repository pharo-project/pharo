initialization
initialize
	super initialize.
	info := MCSliceInfo new.
	info addDependent: self.
	
