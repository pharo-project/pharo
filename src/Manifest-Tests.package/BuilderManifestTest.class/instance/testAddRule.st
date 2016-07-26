tests
testAddRule
	
	| manifestBuilder|
	
	manifestBuilder := TheManifestBuilder of: MFClassA .
	
	self deny: ((manifestBuilder rejectRules) anySatisfy: [:each| each = 0]).
	
	manifestBuilder addRejectRule: 0.
	
	self assert: ((manifestBuilder rejectRules) anySatisfy: [:each| each = 0]).

	manifestBuilder removeRejectRule: 0.	

	self deny: ((manifestBuilder rejectRules) anySatisfy: [:each| each = 0]).
	