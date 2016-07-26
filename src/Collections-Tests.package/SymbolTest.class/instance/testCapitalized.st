tests
testCapitalized

	| uc lc |		
	uc := #MElViN.
	lc := #mElViN.
	self assert:  lc capitalized = uc.
	self assert: uc capitalized = uc.
