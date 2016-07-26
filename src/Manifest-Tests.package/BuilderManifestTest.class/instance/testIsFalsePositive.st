tests
testIsFalsePositive
	
	| manifestBuilder cl mth|
	cl := MFClassA.
	mth := cl >> #method.
	manifestBuilder := TheManifestBuilder of: MFClassA .
	self deny: (manifestBuilder isFalsePositive: mth onRule: 'test' version:0 ).
	
	manifestBuilder addRejectClass: MFClassA.
	self assert: (manifestBuilder isFalsePositive: mth onRule: 'test' version:0 ).
	
	manifestBuilder removeRejectClass: MFClassA.	.
	self deny: (manifestBuilder isFalsePositive: mth onRule: 'test' version:0 ).
	
	manifestBuilder addRejectRule: 'test'.
	self assert: (manifestBuilder isFalsePositive: mth onRule: 'test' version:0 ).
	
	manifestBuilder removeRejectRule: 'test'.
	self deny: (manifestBuilder isFalsePositive: mth onRule: 'test' version:0 ).
	
	manifestBuilder installFalsePositiveOf: 'test' version: 0.
	manifestBuilder addFalsePositive:  mth of: 'test' version: 0.
	self assert: (manifestBuilder isFalsePositive: mth onRule: 'test' version:0 ).
	
	