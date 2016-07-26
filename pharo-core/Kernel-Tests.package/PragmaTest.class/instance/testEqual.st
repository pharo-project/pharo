tests
testEqual

	self assert: atPragma = atPragma.		"Reflexivity"

	self assert: atPragma = anotherAtPragma.		"Simmetry"
	self assert: anotherAtPragma = atPragma.

	self assert: atPragma = anotherAtPragma.		"Transitivity"
	self assert: anotherAtPragma = yetAnotherAtPragma.
	self assert: yetAnotherAtPragma = atPragma.

	self deny: atPragma = atPutPragma.