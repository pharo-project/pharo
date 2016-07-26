tests
testStructAccess

	| struct |
	
	struct := FFITestPackedStructure new.
	self deny: struct getHandle class = ExternalAddress.	
	self deny: struct isNull.
	
	struct byte: 10.
	struct short: -20.
	struct long: 100.
	struct float: 1.0.
	struct double: 2.0.
		self flag: #todo. "This is not yet implemented"
	"struct int64: 123456789101112."
	
	self assert: (struct byte = 10).
	self assert: (struct short = -20).	
	self assert: (struct long = 100).	
	self assert: (struct float = 1.0).	
	self assert: (struct double = 2.0).	
	"self assert: (struct int64 = 123456789101112).	"
	