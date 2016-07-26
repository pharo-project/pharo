tests
testResetFalsePositive
	
	| manifestBuilder array|
	
	array := {MFClassA. MFClassB  }.
	manifestBuilder := TheManifestBuilder of: MFClassA .
	manifestBuilder  installFalsePositiveOf: 'test' version: 0.
	
	manifestBuilder addAllFalsePositive: array of: 'test' version: 0. 
	
	self assert: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [:each| each = MFClassA]).
	self assert: ((manifestBuilder falsePositiveOf: 'test' version: 0) anySatisfy: [:each| each = MFClassB]).
	
	manifestBuilder resetFalsePositiveOf: 'test' version: 0. 
	
	self assert:  (manifestBuilder manifest ruletestV0FalsePositive size = 0).
	
	