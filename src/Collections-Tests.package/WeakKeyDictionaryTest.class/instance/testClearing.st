tests
testClearing
	keys := nil.
	dict at: self put: nil.
	self assert: dict size = (self size + 1).

	Smalltalk garbageCollect.

	"Keys are gone but not yet finalized."
	self assert: dict size = (self size + 1).
	self assert: dict keys size = 1.
	
	dict finalizeValues.
	
	"Now the size should be adjusted."
	self assert: dict size = 1.
	