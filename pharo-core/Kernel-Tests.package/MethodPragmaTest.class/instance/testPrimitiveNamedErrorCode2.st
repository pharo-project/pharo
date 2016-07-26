testing-primitives
testPrimitiveNamedErrorCode2
	"This test useses the #primitiveDirectoryLookup primitive."

	self compile: '<primitive: ''primitiveDirectoryLookup'' module: ''FilePlugin'' error: errorCode> ^ errorCode' selector: #lookup.
	self assert: self lookup equals: nil.
	
