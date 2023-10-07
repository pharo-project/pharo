Class {
	#name : 'MathOperationnsExtensionsTest',
	#superclass : 'TestCase',
	#category : 'Math-Operations-Extensions-Tests',
	#package : 'Math-Operations-Extensions-Tests'
}

{ #category : 'tests' }
MathOperationnsExtensionsTest >> testCloseTo [

	self assert: (3 closeTo: 3.001 precision: 0.001).
	self assert: (0 closeTo: 0.001 precision: 0.001).
	self deny: (3.345 closeTo: 3.35 precision: 0.001).
	
	self deny: (Float nan closeTo: Float nan) description: 'NaN isn''t close to anything'.
	self deny: (Float nan closeTo: 1.0) description: 'NaN isn''t close to anything'.
	self deny: (1.0 closeTo: Float nan) description: 'NaN isn''t close to anything'.

	self deny: (-1.0 closeTo: 1.0).
	self deny: (1.0 closeTo: Float infinity).
	self assert: (Float infinity closeTo: Float infinity) description: 'since they are =, they also are closeTo:'.

	self assert: (1.0/3.0 closeTo: 1/3).
	self assert: (1.0e-8 closeTo: 0).
	self assert: (0 closeTo: 1.0e-8).
	self assert: (1+1.0e-8 closeTo: 1.0).

	self deny: (1000000001 closeTo: 1000000000) description: 'exact representation are considered closeTo: only if equal'
]
