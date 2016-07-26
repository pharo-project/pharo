tests-cycles
testRecursiveArray

	| arr materialized |
	arr := Array new: 3.
	arr at: 1 put: 10.
	arr at: 2 put: 20.
	arr at: 3 put: arr.
	
	materialized := self resultOfSerializeAndMaterialize: arr.

	"We can't check using the equality of the links because hangs ad infinitum. So we only check that the structure is right."
	self assert: materialized first = 10.
	self assert: materialized second = 20.
	self assert: materialized third == materialized.