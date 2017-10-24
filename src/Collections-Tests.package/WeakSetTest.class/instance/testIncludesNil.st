tests
testIncludesNil

	| ws |
	ws := WeakSet new.
	self deny: (ws includes: nil).
	
	"After reclamation, should not includes nil: nil counts for nothing"
	ws add: 1/2.
	ws add: 1/3.
	Smalltalk garbageCollect.
	self deny: (ws includes: nil).