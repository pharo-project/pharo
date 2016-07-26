tests - NaN behavior
testNaN3
   "FloatTest new testNaN3"

   	| set item identitySet |
	set := Set new.
	set add: (item := Float nan).
	self deny: (set includes: item).
	identitySet := IdentitySet new.
	identitySet add: (item := Float nan).
	self assert: (identitySet includes: item).
	"as a NaN is not equal to itself, it can not be retrieved from a set"
