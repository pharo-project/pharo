tests - compare
testCloseTo
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
	
	self assert: (1000000001.0 closeTo: 1000000000.0).
	self deny: (1000000001 closeTo: 1000000000) description: 'exact representation are considered closeTo: only if equal'.