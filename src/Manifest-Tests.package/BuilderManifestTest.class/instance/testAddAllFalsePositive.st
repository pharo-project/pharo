tests
testAddAllFalsePositive
	
	| manifestBuilder array|
	
	array := {MFClassA. MFClassB  }.
	manifestBuilder := TheManifestBuilder of: MFClassA .
	manifestBuilder  installFalsePositiveOf: 'test' version: 0.
	
	self deny: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [:each| each = MFClassA]).
	self deny: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [:each| each = MFClassB]).
	
	manifestBuilder addAllFalsePositive: array of: 'test' version: 0. 
	
	self assert: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [:each| each = MFClassA]).
	self assert: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [:each| each = MFClassB]).
	
	manifestBuilder addAllFalsePositive: array of: 'test' version: 0. 
	
	self assert:  (manifestBuilder manifest ruletestV0FalsePositive size = 2).
	
	manifestBuilder removeAllFalsePositive: array of: 'test' version: 0. 
	
	self deny: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [:each| each = MFClassA]).
	self deny: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [:each| each = MFClassB]).
	
	"manifestBuilder manifest class removeSelector: (manifestBuilder selectorFalsePositiveOf: 'test' version: 0)"