testing-primitives
testPrimitiveNamed1
	"This test useses the #primitiveDirectoryLookup primitive."

	self compile: '<primitive: ''primitiveDirectoryLookup'' module: ''FilePlugin''> ^ #lookup' selector: #lookup.
	self assert: self lookup equals: #lookup.
	
