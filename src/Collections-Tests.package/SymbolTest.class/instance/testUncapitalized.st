tests
testUncapitalized

	| uc lc empty |		
	uc := #MElViN.
	lc := #mElViN.
	empty := #' '.
	self assert:  uc uncapitalized = lc.
	self assert: lc uncapitalized = lc.
	
