running
assertTraitIntegrity
	self assert: TOne traitUsers isHealthy.
	self assert: TOne classTrait traitUsers isHealthy.
	self assert: TTwo traitUsers isHealthy.
	self assert: TTwo classTrait traitUsers isHealthy.