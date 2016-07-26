tests
testCleanUpFP
	| manifestBuilder |
	
	manifestBuilder := TheManifestBuilder of: MFClassA. .
	MFClassA compile:  'foo'.
	manifestBuilder installFalsePositiveOf: 'test' version: 0.
	manifestBuilder addFalsePositive: (MFClassA>>#foo) of:  'test' version: 0. 
	
	self assert: (manifestBuilder containsFalsePositive: (MFClassA>>#foo) onRule: 'test' version: 0).
	
	MFClassA removeSelector: #foo.
	manifestBuilder cleanUp.
	
	self assert: ((manifestBuilder falsePositiveOf: 'test' version: 0 ) size = 0) .
