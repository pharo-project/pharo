accessing
getHandle
	self flag: #todo. "I do not like the name 'getHandle' but I'm looking to unify the 
	API with FFI" 
	^ handle