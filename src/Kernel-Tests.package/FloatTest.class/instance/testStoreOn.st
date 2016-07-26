tests
testStoreOn
	"If storeOn: prints exactly and the parser avoid cumulating round off Errors,
	then Float should be read back exactly.
	Note: there is no guarantee to restore the bit pattern of NaN though"
	
	self assert: (self class compiler evaluate: Float halfPi storeString) = Float halfPi.
	self assert: (self class compiler evaluate: Float halfPi negated storeString) = Float halfPi negated.
	self assert: (self class compiler evaluate: Float infinity storeString) = Float infinity.
	self assert: (self class compiler evaluate: Float infinity negated storeString) = Float infinity negated.
	self assert: (self class compiler evaluate: Float nan storeString) isNaN.