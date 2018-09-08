Extension { #name : #FloatTest }

{ #category : #'*Kernel-Tests-WithCompiler' }
FloatTest >> testStoreOnRoundTrip [
	"If storeOn: prints exactly and the parser avoid cumulating round off Errors,
	then Float should be read back exactly.
	Note: there is no guarantee to restore the bit pattern of NaN though"
	
	self assert: (self class compiler evaluate: Float halfPi storeString) equals: Float halfPi.
	self assert: (self class compiler evaluate: Float halfPi negated storeString) equals: Float halfPi negated.
	self assert: (self class compiler evaluate: Float infinity storeString) equals: Float infinity.
	self assert: (self class compiler evaluate: Float infinity negated storeString) equals: Float infinity negated.
	self assert: (self class compiler evaluate: Float nan storeString) isNaN
]
