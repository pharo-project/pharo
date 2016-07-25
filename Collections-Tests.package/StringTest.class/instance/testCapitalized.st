tests - converting
testCapitalized

	| uc lc empty |		
	uc := 'MElViN'.
	lc := 'mElViN'.
	empty := ' '.
	self assert:  lc capitalized = uc.
	self assert: uc capitalized = uc.
	"the string gets copied"
	self deny: uc capitalized == uc.
	self deny: empty capitalized == empty.