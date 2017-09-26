testing-primitives
testPrimitiveIndexed2
	"This test useses the #identityHash primitive."

	self compile: '<primitive: 75> ^ #idHash' selector: #idHash.
	self assert: self idHash = self basicIdentityHash.