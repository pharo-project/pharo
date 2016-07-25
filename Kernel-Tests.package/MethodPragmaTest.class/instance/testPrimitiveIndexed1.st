testing-primitives
testPrimitiveIndexed1
	"This test useses the #instVarAt: primitive."
	
	self compile: '<primitive: 74> ^ #inst' selector: #inst.
	self assert: self inst = #inst.