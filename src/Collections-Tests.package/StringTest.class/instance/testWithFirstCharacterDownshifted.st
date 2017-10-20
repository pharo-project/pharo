tests - converting
testWithFirstCharacterDownshifted

	| uc lc empty |		
	uc := 'MElViN'.
	lc := 'mElViN'.
	empty := ' '.
	self assert:  uc uncapitalized  = lc.
	self assert: lc uncapitalized = lc.
	"the string gets copied"
	self deny: lc uncapitalized == lc.
	self deny: empty uncapitalized == empty.