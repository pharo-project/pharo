tests
testGrow
	keys := nil.
	dict at: self put: nil.
	Smalltalk garbageCollect.

	"Keys are gone but not yet finalized."
	dict grow.