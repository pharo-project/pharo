tests - identity
testGrowWithNil
	"self run: #testGrowWithNil"
	"This test covers that grow take into account that nil are wrapped elements of sets"
	| set |
	set := IdentitySet new.
	set add: nil.
	set grow.
	self assert: (set includes: nil)