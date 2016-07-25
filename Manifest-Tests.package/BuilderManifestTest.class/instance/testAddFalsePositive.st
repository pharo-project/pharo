tests
testAddFalsePositive
	
	| manifestBuilder cl mth|
	cl := MFClassA.
	mth := MFClassA >> #method.
	manifestBuilder := TheManifestBuilder of: MFClassA .
	manifestBuilder  installFalsePositiveOf: 'test' version: 0.
	
	self deny: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [:each| each = cl]).
	self deny: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [:each| each = mth]).
	
	manifestBuilder addFalsePositive: cl of: 'test' version: 0. 
	manifestBuilder addFalsePositive: mth of: 'test' version: 0. 
	
	self assert: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [:each| each = cl]).
	self assert: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [:each| each = mth]).
	
	manifestBuilder removeFalsePositive: cl of: 'test' version: 0. 
	manifestBuilder removeFalsePositive: mth of: 'test' version: 0. 
	
	self deny: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [:each| each = cl]).
	self deny: ((manifestBuilder falsePositiveOf: 'test'version: 0) anySatisfy: [:each| each = mth]).
