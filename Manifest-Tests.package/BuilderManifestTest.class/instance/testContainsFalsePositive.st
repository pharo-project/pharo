tests
testContainsFalsePositive
	
	| manifestBuilder |
	

	manifestBuilder := TheManifestBuilder of: MFClassA .
	manifestBuilder  installFalsePositiveOf: 'test' version: 0.
	
	manifestBuilder addFalsePositive: MFClassA of: 'test' version: 0. 
	
	self assert: (manifestBuilder containsFalsePositive: MFClassA onRule: 'test' version: 0).
	self deny: (manifestBuilder containsFalsePositive: MFClassB onRule: 'test' version: 0).
	
	
	