testing
testFips180Example3
	"This test is a bit slow if the implementation is not making use of extra primitives"
	self timeLimit: 10 seconds.
	
	hash := SHA256 hashMessage: (String new: 1e6 withAll: $a).
	self assert: hash hex equals: 'cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0'